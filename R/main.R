library(ggplot2)
library(glmnet)
library(MLmetrics)
library(car)
library(cowplot)

data <- read.csv("data/jardin_data.csv")
# Isolate the Date column for further use
dates <- data$Fecha
data <- subset(data, select = -Fecha)
# Convert the data into binary datatype for a correct data split
season_columns <- grep("^Season_", names(data), value = TRUE)
data[season_columns] <- lapply(data[season_columns], function(x) as.integer(x == "True"))

head(data)
summary(data)
dim(data)

# Random splits, 80/20 ratio
set.seed(6)
n <- nrow(data)
train_index <- sample(seq_len(n), size = floor(0.8 * n))
test_index <- setdiff(seq_len(n), train_index)

# Data for Linear Regression
training_set <- data[train_index, ]
test_set <- data[test_index, ]

# Matrices for the penalty-based models
data_features <- subset(data, select = -Caudal)
X_train <- as.matrix(data_features[train_index, ])
X_test <- as.matrix(data_features[test_index, ])
y_train <- training_set$Caudal
y_test <- test_set$Caudal

test_dates <- dates[test_index]
test_actual <- y_test

# Double check the dimensions
dim(training_set)
dim(test_set)
nrow(training_set)
nrow(test_set)

# Function for  Nash-Sutcliffe Efficiency
NSE <- function(actual, pred){
  numerator <- sum((actual - pred)^2)
  denominator <- sum((actual - mean(actual))^2)
  
  if (denominator == 0){
    return(1)
  }

  1 - (numerator / denominator)
}

# --- Linear Regression ---
model_linear <- lm(Caudal ~ ., data = training_set)
summary(model_linear)

# Predictions
model_linear_predictions <- predict(model_linear, newdata = test_set)

# Create a plotting data frame for convenience
plot_data_linear <- data.frame(
  Fecha = as.Date(test_dates),
  Actual = test_actual,
  Predicted = model_linear_predictions
)

# Plot Linear Regression Model
plot_linear_model <- ggplot(plot_data_linear, aes(x = Fecha)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.5) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "lightblue", "Predicted" = "firebrick")) +
  labs(
    title = "Actual vs Predicted Caudal (Linear Model)",
    x = "Date (Fecha)",
    y = "Caudal",
    color = "Legend"
  ) +
  theme_minimal()

rmse_linear <- RMSE(model_linear_predictions, y_test)
nse_linear <- NSE(y_test, model_linear_predictions)


# Ridge, Lasso and Elastic Net Section
# --- RIDGE MODEL ---
model_ridge <- cv.glmnet(X_train, y_train, alpha=0.0)
best_lambda_ridge <- model_ridge$lambda.min
model_ridge <- glmnet(X_train, y_train , alpha=0.0, lambda=best_lambda_ridge) # fit the bets lambda
summary(model_ridge)

model_ridge_predictions <- predict(model_ridge, X_test)

# Plotting data for Ridge model
plot_data_ridge <- data.frame(
  Fecha=as.Date(test_dates),
  Actual=test_actual,
  Predicted = as.vector(model_ridge_predictions)
)

# Plot ridge model
plot_ridge_model <- ggplot(plot_data_ridge, aes(x=Fecha))+
  geom_line(aes(y=Actual, color='Actual'), linewidth=1.5)+
  geom_line(aes(y=Predicted, color="Predicted"), linewidth=1.5, linetype="dashed")+
  scale_color_manual(values=c("Actual"='lightblue', "Predicted"='orange'))+
  labs(
    title="Actual vs Predicted Caudal (Ridge Model)",
    x='Date(Fecha)',
    y='Caudal',
    color='Legend'
  )+
  theme_minimal()

rmse_ridge <- RMSE(model_ridge_predictions, y_test)
nse_ridge <- NSE(y_test, model_ridge_predictions)

rmse_ridge
nse_ridge

# --- LASSSO ---
model_lasso <- cv.glmnet(X_train, y_train, alpha=1.0)
best_lambda_lasso <- model_lasso$lambda.min
model_lasso <- glmnet(X_train, y_train , alpha=1.0, lambda=best_lambda_lasso) # fit the bets lambda
summary(model_lasso)

model_lasso_predictions <- predict(model_lasso, X_test)

plot_data_lasso <- data.frame(
  Fecha=as.Date(test_dates),
  Actual=test_actual,
  Predicted = as.vector(model_lasso_predictions)
)

# plot lasso model
plot_lasso_model <- ggplot(plot_data_lasso, aes(x=Fecha))+
  geom_line(aes(y=Actual, color='Actual'), linewidth=1.5)+
  geom_line(aes(y=Predicted, color="Predicted"), linewidth=1.5, linetype="dashed")+
  scale_color_manual(values=c("Actual"='lightblue', "Predicted"='purple'))+
  labs(
    title="Actual vs Predicted Caudal (Lasso Model)",
    x='Date(Fecha)',
    y='Caudal',
    color='Legend'
  )+
  theme_minimal()

rmse_lasso <- RMSE(model_lasso_predictions, y_test)
nse_lasso <- NSE(y_test, model_lasso_predictions)

rmse_lasso
nse_lasso


# ---Elastic Net Model---
model_elast_net <- cv.glmnet(X_train, y_train, alpha=0.5)
best_lambda_elast_net <- model_elast_net$lambda.min
model_elast_net <- glmnet(X_train, y_train, alpha=0.5, lambda=best_lambda_elast_net)

summary(model_elast_net)

model_elastic_net_predictions <- predict(model_elast_net, X_test)

plot_data_elast_net <- data.frame(
  Fecha= as.Date(test_dates),
  Actual=test_actual,
  Predicted=as.vector(model_elastic_net_predictions)
)

plot_elastic_net_model <- ggplot(plot_data_elast_net, aes(x=Fecha))+
  geom_line(aes(y=Actual, color='Actual'), linewidth=1.5)+
  geom_line(aes(y=Predicted, color="Predicted"), linewidth=1.5, linetype="dashed")+
  scale_color_manual(values=c("Actual"='lightblue', "Predicted"='blue'))+
  labs(
    title="Actual vs Predicted Caudal (Elastic Net Model)",
    x='Date(Fecha)',
    y='Caudal',
    color='Legend'
  )+
  theme_minimal()

rmse_net_elastic_net <- RMSE(model_elastic_net_predictions, y_test)
nse_elastic_net <- NSE(y_test, model_elastic_net_predictions)

plots <- list(linear_plot = plot_linear_model, ridge_plot = plot_ridge_model, lasso_plot = plot_lasso_model, elast_net_plot = plot_elastic_net_model)

# Create a grid of plots
plot_grid(plotlist = plots, ncol=2,nrow=2)

# --- KEY PERFORMANCE METRICS AND SUMMARY ---
# NSE
nse_all <- list(lasso_nse = nse_lasso, ridge_nse=nse_ridge, linear_nse = nse_linear, elastic_net_nse = nse_elastic_net)
# RMSE
rmse_all <- list(lasso_rmse=rmse_lasso, ridge_rmse=rmse_ridge, linear_rmse=rmse_linear, elastic_net_rmse = rmse_net_elastic_net)

print(nse_all)
print(rmse_all)

# SUMMARIZING
model_metrics <- data.frame(
  Model = c("Multiple Linear Regression", "Ridge", "LASSO", "Elastic Net"),
  RMSE = c(rmse_linear, rmse_ridge, rmse_lasso, rmse_net_elastic_net),
  NSE = c(nse_linear, nse_ridge, nse_lasso, nse_elastic_net)
)

best_rmse <- model_metrics[which.min(model_metrics$RMSE), ]
best_nse <- model_metrics[which.max(model_metrics$NSE), ]

cat("\nModel comparison:\n")
print(model_metrics)

cat(sprintf(
  "\nBest model by RMSE: %s (RMSE = %.4f)\n",
  best_rmse$Model,
  best_rmse$RMSE
))

cat(sprintf(
  "Best model by NSE: %s (NSE = %.4f)\n",
  best_nse$Model,
  best_nse$NSE
))