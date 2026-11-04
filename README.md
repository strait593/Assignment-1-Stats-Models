# Assignment 1: Statistical Models in Artificial Intelligence

## Project Title
Predicting river flow (caudal) in the Betania basin using statistical regression models.

## Course
Statistical Models in Artificial Intelligence

## Objective
The goal of this assignment is to build and compare predictive models for daily river discharge (Caudal) using hydrological and meteorological covariates from the Betania basin dataset. The models are evaluated using RMSE and Nash-Sutcliffe Efficiency (NSE), and the best-performing model is selected based on predictive performance.

The assignment requires:
- loading and preparing the dataset;
- splitting data into training and test sets using an 80/20 random split;
- predicting Caudal from all remaining variables excluding the Fecha column;
- fitting multiple linear regression, ridge regression, LASSO, and elastic net models;
- selecting the best model according to RMSE and NSE;
- implementing a custom NSE function.

## Dataset
The project uses the dataset stored in:

- `data/jardin_data.csv`

This dataset contains daily observations from the Betania basin, including precipitation and hydrological covariates. The target variable is:

- `Caudal`

The date column is:

- `Fecha`

The `Fecha` column is excluded from the model features but is retained for plotting and temporal interpretation.

## Problem Formulation
We model the river discharge as a supervised regression problem:

Caudal = f(X)

where X contains the remaining predictors (all variables except `Fecha` and the target itself).

## Methodology

### 1. Data loading and preprocessing
- Load the CSV file from `data/jardin_data.csv`
- Remove the `Fecha` column from feature selection
- Keep it only for plotting the test predictions over time
- Ensure the data is split into feature matrix and target vector

### 2. Train/test split
A random 80/20 split is used:

- 80% of observations for training
- 20% for testing

This is done using a reproducible random seed in the prototype implementation.

### 3. Models implemented
The following models are compared:

1. Multiple Linear Regression
   - `lm(Caudal ~ ., data = train_data)`

2. Ridge Regression
   - regularized linear model with alpha = 0
   - tuned using cross-validation to select the best lambda

3. LASSO Regression
   - regularized linear model with alpha = 1
   - tuned with cross-validation

4. Elastic Net Regression
   - regularized linear model with alpha = 0.5
   - tuned using cross-validation

### 4. Evaluation metrics
The models are assessed using:

- RMSE (Root Mean Squared Error)
- Nash-Sutcliffe Efficiency (NSE)

#### RMSE
`RMSE = sqrt((1/n) * sum((y_i - yhat_i)^2))`

Lower RMSE indicates better performance.

#### NSE
The assignment requires a custom implementation of the Nash-Sutcliffe Efficiency:

`NSE = 1 - [sum((y_i - yhat_i)^2) / sum((y_i - mean(y))^2)]`

NSE values closer to 1 indicate stronger predictive performance. A value of 1 represents a perfect prediction.

## Project Structure

```text
Assignment-1/
├── Assignment-1.Rproj
├── R/
│   ├── main.R
│   └── prototype.R
├── data/
│   └── jardin_data.csv
├── notebooks/
│   └── python_implementation.ipynb
├── plots/
├── requirements.md
├── Rplots.pdf
└── README.md
```

## File Descriptions

- `R/main.R`: main R implementation of the assignment pipeline
- `R/prototype.R`: exploratory/prototype script with model comparisons and plots
- `data/jardin_data.csv`: dataset used for training and testing
- `notebooks/python_implementation.ipynb`: Python implementation of the workflow
- `plots/`: directory for generated visualizations
- `requirements.md`: assignment requirements summary
- `Rplots.pdf`: saved plot output from the R analysis

## Installation and Setup

### Requirements
The project is primarily implemented in R and uses the following packages:

- `ggplot2`
- `glmnet`
- `MLmetrics`
- `car`

### Install dependencies in R
Open R or RStudio and run:

```r
install.packages(c("ggplot2", "glmnet", "MLmetrics", "car"))
```

## How to Run

### Option 1: Run the main script
From the project root, launch the main script in R:

```r
source("R/main.R")
```

Or run it directly from the command line:

```bash
Rscript R/main.R
```

### Option 2: Run the prototype script
The prototype script contains a more exploratory version of the full workflow and produces plots for each model:

```r
source("R/prototype.R")
```

## Expected Outputs
The project produces:

- model predictions on the test set;
- RMSE and NSE values for each model;
- comparison of the best model according to each criterion;
- visual plots comparing actual versus predicted river flow for each method.

This includes outputs for:
- Linear Regression
- Ridge Regression
- LASSO
- Elastic Net

## Selection of the Best Model
The assignment asks for a model comparison based on prediction quality. Typically, the best model is chosen as:

- the model with the lowest RMSE for the test set;
- the model with the highest NSE value for the test set.

The report should mention both results and the final model selected according to the criteria above.

## Notes on Interpretation
In hydrological forecasting, model quality depends on how closely predictions follow observed streamflow over time. A model with a lower RMSE and a higher NSE is generally preferred because it captures the variability of river discharge more accurately and reduces large deviations between observed and predicted values.

## Reproducibility
To make the analysis reproducible:

- use the same train/test split ratio;
- set a random seed before sampling the data; and
- run the same script in the same environment with consistent package versions.

## Summary
This assignment demonstrates the use of statistical learning techniques for hydrological forecasting. By comparing multiple regression models and evaluating them with RMSE and custom NSE, it highlights how regularization methods such as Ridge, LASSO, and Elastic Net can improve predictive performance and reduce overfitting compared to ordinary least squares regression.

## License
This project is intended for academic coursework and educational use.

## Author / Contributor
This repository was created for the Statistical Models in Artificial Intelligence assignment.
