---
title: "Term Project: Fall 2021"
subtitle: "Predictive Model Building"
author: "Ian Effendi"
course: "STAT 745"
instructor: "Professor Bajorski"
---

## Objective

> Create a predictive model that predicts the response $Y$ based on the predictors $X \in {X_1, X_2, \dots, X_140}$.

## Provided Files

The course instructor has provided three files:

- [`Data.train.RData`]("data-raw/Data.train.RData")
- [`Data.test.RData`]("data-raw/Data.train.RData")
- [`Bajorski.Predictions.csv`]("data-raw/Bajorski.Predictions.csv")

## Instructions

A labelled training sample ($n = 5200, p = 140$) is provided in a $5200$x$141$ data frame stored within `Data.train`. A corresponding testing sample ($n = 5200, p = 140$) is provided within a similar data frame, `Data.test`.

No additional context or information is provided in regards to what the data means.

There are three main tasks to accomplish:

### 1. Make the Predictive Model

Create a predictive model in a file with `R` code named `<LASTNAME>.Code.R` that calculates your final predictions.

The code should take input from files `Data.train.RData` and `Data.test.RData` from the working directory and output the `<LASTNAME>.Predictions.csv` file described in the next point.

The code should not include any exploratory data analysis,
CV, or other preliminary work. It should just use the final predictive model or algorithm. Since the assessment of the test `MSE` may depend on the cross-validation, you can use something like `MSE <- 2000` in your code using the previously calculated value.

The file should be tested as follows:

1. Delete all objects in your workspace.
2. Run `source("<LASTNAME>.Code.R")`.
3. Verify `<LASTNAME>.Predictions.csv` has been appropriately created.

### 2. Format Predictions

Ensure the created `<LASTNAME>.Predictions.csv` file contains:

- Your last name in the first line.
- A single number representing your guess about the `MSE` of the test dataset in the second line.
- Predictions for the response in the test dataset, in the order of the observations as given, with one value per line.

See `Bajorski.Predictions.csv` for more information on how the format should look.

### 3. Write Report

The final report should be in a file named `<LASTNAME>_Final_Report` which should contain a brief description of what you were doing in this project to get the best predictions. The report should be up to 3 pages of single-spaced text; preferably in a Word document.

Included with this report should be one guess about the `MSE` you will obtain on the test dataset. (The best possible value is $\approx 4$ but values under $10$ are very good). A straightforward linear regression gives an `MSE` $\geq 300000$, so any value lower than this is better.

The final grade partially considers the accuracy of your test `MSE`, but you will also be graded on effort, variety, and depth of approach.

## Deliverables

1. `<LASTNAME>_Final_Report(.doc|.docx|.pdf)`, up to 3 pages, single-spaced.
2. `<LASTNAME>.Code.R`.
3. `<LASTNAME>.Predictions.csv`.
