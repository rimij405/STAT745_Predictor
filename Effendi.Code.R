# Effendi.Code.R
#
# Handles loading data subsets, fitting a predictive
# model on the training samples, making predictions
# using the testing samples, and exporting results
# into the `Effendi.Predictions.csv` file.

## Imports ----

# Required libraries...
library(tictoc)
library(here)
library(readr)
library(dplyr)
library(glmnet)

## Environment ----

# Prepare the Utils environment.
Utils <<- new.env(parent = emptyenv())

# Prepare the Analysis environment.
Model <<- new.env(parent = emptyenv())

# Prepare the Data environment.
Data <<- new.env(parent = emptyenv())

## Utilities ----

#' Write message if verbose flag is set to `TRUE`.
#' @param verbose Display verbose log message(s)?
Utils$log <- function(..., verbose = FALSE) {
    if(verbose) {
        message(...)
    }
}

#' List contents of supplied environment.
#' @param envir Environment object to list.
#' @return String containing environment contents.
Utils$ls.env <- function(envir = parent.frame()) {
    label <- paste0(substitute(envir), "$")
    props <- paste0(
        label,
        ls(envir),
        collapse = ", "
    )
    return(props)
}

#' Import data from the specified filepaths.
#' @param file Filename of data file to load.
#' @param envir Environment that will store the loaded data.
#' @param verbose Display verbose messages?
#' @return Environment with data.frame containing loaded data.
Utils$import.data <- function(file = "Data.train.RData", envir = Data, verbose = FALSE) {
    tryCatch(
        expr = {
            # Begin timer.
            tic(sprintf("Loading data in '%s'.", file), quiet = !verbose)
            # Get the input filepath.
            input_filepath = here::here(file)
            # Check if file exists.
            stopifnot("File does not exist at specified location." = file.exists(input_filepath))
            # Obtain the file.
            load(file = input_filepath, envir = envir, verbose = verbose)
            # Return the environment with the loaded data.
            Utils$log("Loaded data into environment.", verbose = verbose)
            Utils$log(Utils$ls.env(envir), verbose = verbose)
        },
        finally = {
            # End timer.
            toc(quiet = !verbose)
        }
    )
    # Return the environment.
    return(envir)
}

#' Load the training samples.
#' @param envir Environment to store loaded data.
#' @param verbose Display verbose messages?
Utils$import.data.train <- function(envir = Data, verbose = FALSE) {
    # Check if data subset already exists.
    if(exists("Data.train", envir = envir)) {
        Utils$log("Existing data found in provided environment. Returning cached value.", verbose = verbose)
        return(invisible(envir$Data.train))
    }

    # If not, return data.
    Utils$import.data(file = "Data.train.RData", envir = envir, verbose = verbose)
    stopifnot("Error loading data subset into environment." = exists("Data.train", envir = envir))
    return(invisible(envir$Data.train))
}

#' Load the testing samples.
#' @param envir Environment to store loaded data.
#' @param verbose Display verbose messages?
Utils$import.data.test <- function(envir = Data, verbose = FALSE) {
    # Check if data subset already exists.
    if(exists("Data.test", envir = envir)) {
        Utils$log("Existing data found in provided environment. Returning cached value.", verbose = verbose)
        return(invisible(envir$Data.test))
    }

    # If not, return data.
    Utils$import.data(file = "Data.test.RData", envir = envir, verbose = verbose)
    stopifnot("Error loading data subset into environment." = exists("Data.test", envir = envir))
    return(invisible(envir$Data.test))
}

#' Export predictions to the specified file.
#' @param .labels Vector containing predictions.
#' @param MSE_test Estimated test MSE score.
#' @param author Name of the author.
#' @param output_dir Output directory folder.
#' @param output_file Output filename.
Utils$export.predictions <- function(.labels = rep(0, times=10),
                                     MSE_test = 2000,
                                     author = "Ian Effendi",
                                     ...,
                                     output_dir = "",
                                     output_file = "Effendi.Predictions.csv",
                                     verbose = FALSE) {
    tryCatch(
        expr = {
            # Begin clock.
            tic("Exporting predictions...", quiet = !verbose)

            # Prepare the output file.
            output_filepath <- here::here(output_dir, output_file)

            # Create a new vector for the output.
            output <- c(author, MSE_test, .labels)

            # Save as a comma-separated value document.
            write.table(output, file = output_filepath, row.names = FALSE, col.names = FALSE, sep = ",")
            message(sprintf("Saved %i prediction(s) to output file '%s'", length(.labels), output_filepath))
        },
        finally = {
            # End clock.
            toc(quiet = !verbose)
        }
    )
}

## Model ----

### Outlier Removal ----

# Prepare the preprocess environment.
Model$prep <- new.env(parent = Model)

#' Calculate outliers and remove them.
Model$prep$fit_transform <- function(X, y) {

    # Prepare the data.
    df.raw <- data.frame(Y = y, X)

    # Fit model with all predictors.
    lm.base <- lm(Y ~ ., data = df.raw, x = TRUE)

    # Calculate high leverage points and residuals.
    large.Cooks <- cooks.distance(lm.base) > (4/nrow(df.raw))
    large.Residuals <- rstudent(lm.base) > 3

    # Get the cleaned data.
    df.clean <- df.raw[!large.Cooks & !large.Residuals,]

    # Count of removed rows.
    n_removed = nrow(df.raw) - nrow(df.clean)
    message(sprintf("Number of outlier records removed: %d", n_removed))

    # Return the Y and features.
    return(df.clean)

}

### LASSO Regression ----

# Prepare the regressor environment.
Model$clf <- new.env(parent = Model)

#' Select the best Lambda for LASSO regression through k-fold cross-validation.
Model$clf$cv.lambda.min <- function(X_train, y_train, lambdas, k = 5) {
    tryCatch(
        expr = {
            # Begin clock.
            tic("Selecting best lambda for LASSO through cross-validation.", quiet = FALSE)
            Utils$log(sprintf("Performing %d-Fold Cross-Validation...", k))

            # Setting alpha = 1 for LASSO.
            # Select best lambda using regression.
            cv.lasso <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = k)

            # Select best lasso.
            lambda.best <- cv.lasso$lambda.min
        },
        finally = {
            # End clock.
            toc(quiet = FALSE)
        }
    )
    return(lambda.best)
}

#' Fit the classifier.
#' @param X_train Training features.
#' @param y_train Training labels.
#' @return List containing the fit model and the best lambda for LASSO regression.
Model$clf$fit <- function(X_train, y_train) {
    tryCatch(
        expr = {
            # Begin clock.
            tic("Fitting LASSO regressor on training samples.", quiet = FALSE)

            # Prepare lambdas for LASSO regression.
            lambdas <- 10^seq(2, -3, by = -.1)

            # Get the best lambda.
            lambda.best <- Model$clf$cv.lambda.min(X_train, y_train, lambdas = lambdas)

            # Fit model with best lasso.
            model.fit <- glmnet(X_train, y_train, alpha = 1, lambda = lambda.best, standardize = TRUE)

            # Print model summary and results.
            print(model.fit)
            print(summary(model.fit))
        },
        finally = {
            # End clock.
            toc(quiet = FALSE)
        }
    )
    return(list(
        fit = model.fit,
        lambda.best = lambda.best
    ))
}

#' Make predictions using the `newdata`.
#' @param model.fit Fit model to make predictions with.
Model$clf$predict <- function(model.fit, ...) {
    model.predictions <- predict(model.fit, ...)
    return(model.predictions)
}

#' Fit new model and make predictions for test data.
#' @param X_train Training features.
#' @param y_train Training labels.
#' @param X_test Testing features.
#' @return List containing training and test predictions.
Model$clf$fit_predict <- function(X_train, y_train, X_test) {
    # Convert data to matrices.
    X_train <- as.matrix(X_train)
    X_test <- as.matrix(X_test)

    # Fit the model.
    model.obj <- Model$clf$fit(X_train, y_train)

    # Make predictions.
    predictions.train <- Model$clf$predict(model.obj$fit, s = model.obj$lambda.best, newx = X_train)
    predictions.test <- Model$clf$predict(model.obj$fit, s = model.obj$lambda.best, newx = X_test)

    # Return results.
    return(list(
        train.preds = predictions.train,
        test.preds = predictions.test,
        lambda.best = model.obj$lambda.best
    ))
}

#' Calculate training error metrics for classifier results.
Model$clf$metrics <- function(y_truth, y_preds) {
    residuals <- (y_preds - y_truth)
    df.error <- data.frame(
        Metric = c("MSE",
                   "RMSE",
                   "MAE"),
        Error = c(
            mean(residuals**2),
            sqrt(mean(residuals**2)),
            mean(abs(residuals))
        )
    )
    print(df.error)
    return(df.error)
}

## Interactive ----

# Main function for execution of code.
main <- function(verbose = FALSE) {
    tryCatch(
        expr = {
            # Begin clock.
            tic("Making predictions.", quiet = !verbose)

            # Get the data.
            Utils$import.data.train(envir = Data, verbose = verbose)
            Utils$import.data.test(envir = Data, verbose = verbose)

            # Subset training and testing data.
            tic("Subsetting the data.", quiet = !verbose)
            X_train <- Data$Data.train %>% select(-Y)
            y_train <- Data$Data.train %>% pull(Y)
            X_test <- Data$Data.test
            y_test <- rep(0, times = nrow(X_test))
            toc(quiet = !verbose)

            # Preprocess data to remove outliers.
            tic("Preprocess training data.", quiet = !verbose)
            Xy_train <- Model$prep$fit_transform(X_train, y_train)
            X_train <- Xy_train %>% select(-Y)
            y_train <- Xy_train %>% pull(Y)
            toc(quiet = !verbose)

            # Fit model and make predictions.
            results <- Model$clf$fit_predict(X_train, y_train, X_test)
            y_test <- results$test.preds

            # Calculate the training errors
            # and estimate the testing error.
            tic("Estimating test error.", quiet = !verbose)

            print(sprintf("Best lambda = %.4f", results$lambda.best))
            print(sprintf("Average response = %.4f", mean(results$train.preds)))

            Error.train <- Model$clf$metrics(y_train, results$train.preds)
            MSE_test <- Error.train %>%
                filter(Metric == "MSE") %>%
                pull(Error) %>%
                sum(. * 0.25) %>%
                round(0)
            toc(quiet = !verbose)

            # Make predictions.
            output_file = "Effendi.Predictions.csv"
            Utils$export.predictions(
                .labels = y_test,
                MSE_test = MSE_test,
                author = "Ian Effendi",
                output_file = output_file
            )

            # View the created predictions.
            View(y_test)
        },
        error = (function(cond) {
            message(cond)
        }),
        finally = {
            # End clock.
            toc(quiet = !verbose)
        }
    )
}

# Execute the code.
main(verbose = TRUE)
