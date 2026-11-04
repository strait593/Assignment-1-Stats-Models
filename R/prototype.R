library(ggplot2)
library(glmnet)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

nash_sutcliffe_efficiency <- function(actual, predicted) {
  numerator <- sum((actual - predicted)^2)
  denominator <- sum((actual - mean(actual))^2)

  if (denominator == 0) {
    return(1)
  }

  1 - (numerator / denominator)
}

# Read data and clean missing values
df <- read.csv("data/jardin_data.csv")
df <- na.omit(df)

# Ensure the date column is kept for plotting, then remove it from model covariates
fecha_vector <- as.Date(df$Fecha)
df_features <- subset(df, select = -Fecha)

# Convert logical-like columns to numeric because glmnet requires numeric matrices
is_logical_like <- function(x) {
  if (is.logical(x)) {
    return(TRUE)
  }

  if (is.character(x)) {
    values <- unique(na.omit(x))
    if (length(values) == 0) {
      return(FALSE)
    }
    return(all(toupper(values) %in% c("TRUE", "FALSE", "T", "F")))
  }

  FALSE
}

logical_like_cols <- sapply(df_features, is_logical_like)
if (any(logical_like_cols)) {
  df_features[logical_like_cols] <- lapply(df_features[logical_like_cols], function(x) {
    if (is.logical(x)) {
      return(as.numeric(x))
    }
    as.numeric(toupper(x) %in% c("TRUE", "T"))
  })
}

# Random 80/20 train/test split
set.seed(6)
train_index <- sample(seq_len(nrow(df_features)), size = floor(0.8 * nrow(df_features)))
train_df <- df_features[train_index, ]
test_df <- df_features[-train_index, ]

# Separate target and covariates
X_train <- as.matrix(subset(train_df, select = -Caudal))
y_train <- train_df$Caudal
X_test <- as.matrix(subset(test_df, select = -Caudal))
y_test <- test_df$Caudal

# Linear regression model
model_linear <- lm(Caudal ~ ., data = train_df)
linear_pred <- predict(model_linear, newdata = test_df)

# Ridge regression model
model_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
ridge_pred <- as.numeric(predict(model_ridge, newx = X_test, s = "lambda.min"))

# Lasso regression model
model_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
lasso_pred <- as.numeric(predict(model_lasso, newx = X_test, s = "lambda.min"))

# Elastic net model
model_elastic_net <- cv.glmnet(X_train, y_train, alpha = 0.5)
elastic_pred <- as.numeric(predict(model_elastic_net, newx = X_test, s = "lambda.min"))

# Create a metrics table
results <- data.frame(
  Model = c("Linear", "Ridge", "Lasso", "Elastic Net"),
  RMSE = c(
    rmse(y_test, linear_pred),
    rmse(y_test, ridge_pred),
    rmse(y_test, lasso_pred),
    rmse(y_test, elastic_pred)
  ),
  NSE = c(
    nash_sutcliffe_efficiency(y_test, linear_pred),
    nash_sutcliffe_efficiency(y_test, ridge_pred),
    nash_sutcliffe_efficiency(y_test, lasso_pred),
    nash_sutcliffe_efficiency(y_test, elastic_pred)
  )
)

best_rmse_model <- results[which.min(results$RMSE), ]
best_nse_model <- results[which.max(results$NSE), ]

cat("Model performance on the test set:\n")
print(results[order(results$RMSE), ])

cat("\nBest model by RMSE:")
print(best_rmse_model)

cat("\nBest model by NSE:")
print(best_nse_model)

# Create plotting data
test_dates <- fecha_vector[-train_index]
plot_data_linear <- data.frame(
  Fecha = test_dates,
  Actual = y_test,
  Predicted = linear_pred
)
plot_data_ridge <- data.frame(
  Fecha = test_dates,
  Actual = y_test,
  Predicted = ridge_pred
)
plot_data_lasso <- data.frame(
  Fecha = test_dates,
  Actual = y_test,
  Predicted = lasso_pred
)
plot_data_elastic_net <- data.frame(
  Fecha = test_dates,
  Actual = y_test,
  Predicted = elastic_pred
)

# Plot the models against the actual data
linear_plot <- ggplot(plot_data_linear, aes(x = Fecha)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Linear Regression: Actual vs. Predicted Caudal",
    x = "Date",
    y = "Caudal",
    color = "Series"
  ) +
  theme_minimal()

ridge_plot <- ggplot(plot_data_ridge, aes(x = Fecha)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Ridge Regression: Actual vs. Predicted Caudal",
    x = "Date",
    y = "Caudal",
    color = "Series"
  ) +
  theme_minimal()

lasso_plot <- ggplot(plot_data_lasso, aes(x = Fecha)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Lasso Regression: Actual vs. Predicted Caudal",
    x = "Date",
    y = "Caudal",
    color = "Series"
  ) +
  theme_minimal()

elastic_plot <- ggplot(plot_data_elastic_net, aes(x = Fecha)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Elastic Net Regression: Actual vs. Predicted Caudal",
    x = "Date",
    y = "Caudal",
    color = "Series"
  ) +
  theme_minimal()

print(linear_plot)
print(ridge_plot)
print(lasso_plot)
print(elastic_plot)

# save the plots
plots <- c(linear_plot, ridge_plot, lasso_plot, elastic_plot)
savePlot(plots)