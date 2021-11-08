# Effendi.Code.R
#
# Handles loading training and testing samples,
# fits a predictive model on the training samples,
# makes a prediction using the testing samples,
# and exports the information into `Effendi.Predictions.csv`

## ---- Imports ----

# Required libraries:
library(readr)
library(here)

## ---- Exports ----

#' Export predictions to the specified file.
#' @param .labels Vector containing predictions.
#' @param MSE_test Estimated test MSE score.
#' @param author Name of the author.
#' @param output_dir Output directory folder.
#' @param output_file Output filename.
export.predictions <- function(.labels = rep(0, times=10), MSE_test = 2000, author = "Ian Effendi", ..., output_dir = "", output_file = "Effendi.Predictions.csv") {
    # Prepare the output file.
    output_filepath <- here::here(output_dir, output_file)

    # Create a new vector for the output.
    output <- c(author, MSE_test, .labels)

    # Save as a comma-separated value document.
    write.table(output, file = output_filepath, row.names = FALSE, col.names = FALSE, sep = ",")
    message(sprintf("Saved %i prediction(s) to output file '%s'", length(.labels), output_filepath))
}

## ---- Helpers ----

#' Describe an environment's contents.
#' @param envir Environment variable.
#' @return String containing environment contents.
ls.env <- function(envir = emptyenv()) {
    label <- paste0(substitute(envir), "$")
    props <- paste0(label, ls(envir), collapse = ", ")
    return(props)
}

## ---- Loaders ----

#' Load a data file into the given context and return the environment.
#' @param file Filename of data file to load.
#' @param ctx Context environment that will store the loaded data for recovery upon return.
#' @param verbose Display verbose loading messages?
#' @return Contextual environment containing any loaded data.
load.data <- function(file = "Data.train.RData", ..., ctx = new.env(parent = emptyenv()), verbose = FALSE) {
    # Get the input filepath.
    input_filepath = here::here(file)

    # Check if file exists.
    if(!file.exists(input_filepath)) {
        stop(sprintf("Failed to load data file. No file exists at location '%s'.", input_filepath))
    }

    # Obtain the file.
    load(file = input_filepath, envir = ctx, verbose = verbose)

    # Return the data environment.
    message(paste0("Loaded data into the context environment: ", ls.env(ctx)))
    return(ctx)
}

#' Load the training samples.
#' @param ctx Environment to store loaded data within.
#' @param verbose Display verbose loading messages?
load.data.train <- function(ctx = new.env(parent = emptyenv()), verbose = FALSE) {
    file = "Data.train.RData"

    # If the environment already has data cached, just return that.
    if(exists("Data.train", envir = ctx)) {
        message("Found existing data in provided environment. Returning cached value.")
        return(invisible(ctx$Data.train))
    }

    # If not already loaded, load the data.
    load.data(file = file, ctx = ctx, verbose = verbose)
    if(exists("Data.train", envir = ctx)) {
        return(invisible(ctx$Data.train))
    }

    # If not loaded still, error.
    stop("Cannot load data. No `Data.train` loaded into memory.")
}

#' Load the testing samples.
#' @param ctx Environment to store loaded data within.
#' @param verbose Display verbose loading messages?
load.data.test <- function(ctx = new.env(parent = emptyenv()), verbose = FALSE) {
    file = "Data.test.RData"

    # If the environment already has data cached, just return that.
    if(exists("Data.test", envir = ctx)) {
        message("Found existing data in provided environment. Returning cached value.")
        return(invisible(ctx$Data.test))
    }

    # If not already loaded, load the data.
    load.data(file = file, ctx = ctx, verbose = verbose)
    if(exists("Data.test", envir = ctx)) {
        return(invisible(ctx$Data.test))
    }

    # If not loaded still, error.
    stop("Cannot load data. No `Data.test` loaded into memory.")
}

## ---- Model ----

#' Fit a model and return an environment containing the model fit results.
#' @param X.train Training features.
#' @param y.train Labelled repsonse.
clf.fit <- function(X.train, y.train) {
    # Prepare a data.frame.
    df <- data.frame(cbind(X.train, Y = y.train))

    # Prepare a model fit on training data.
    lm.fit <- lm(Y ~ ., data = df)

    # Return the fit model.
    return(lm.fit)
}

#' Predict response for a set of unlabelled test observations.
#' @param X.test A set of observations with data from 140 predictors.
#' @param clf A previously fit model.
clf.predict <- function(X.test, fit.model) {
    predictions <- predict(fit.model, newdata = X.test)
    return(predictions)
}

#' Fit a model and make predictions against a set of test values.
#' @param X.train Training features.
#' @param y.train Labelled repsonse.
#' @param X.test A set of observations with data from 140 predictors.
clf.fit_predict <- function(X.train, y.train, X.test) {
    fit.model <- clf.fit(X.train, y.train)
    predictions <- clf.predict(X.test, fit.model)
    export.predictions(.labels = predictions)
}

## ---- Main ----

# Load the training and testing data.
e <- new.env(parent = emptyenv())
load.data.train(ctx = e)
load.data.test(ctx = e)

# Subset the data into train and test splits.
e$X.train <- e$Data.train[, -1] # Select all training features.
e$y.train <- e$Data.train[, 1] # Select response values.
e$X.test <- e$Data.test # Select the test features.

# Fit a model.
e$model <- clf.fit(e$X.train, e$y.train)
summary(e$model)

# Make predictions.
e$predictions <- clf.predict(e$X.test, e$model)
e$MSE <- 2000 # Estimated guessed MSE.

# Export predictions.
export.predictions(.labels = e$predictions, MSE_test = e$MSE)
