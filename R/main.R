library(ggplot2)
library(glmnet)
library(MLmetrics)
library(car)

data <- read.csv("data/jardin_data.csv")
# Isolate the Date column for further use
dates <- data$Fecha
data <- subset(data, select = -Fecha)

head(data)
summary(data)
dim(data)

# Calculate sizes of train/test splits
n <- nrow(data)
training_size <- floor(n * 0.8)

# Train/test split with 80/20 ratio
training_set <- data[1:training_size, ]
test_set     <- data[(training_size + 1):n, ]

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

test_dates <- dates[(training_size + 1):nrow(data)]
test_actual <- data$Caudal[(training_size + 1):nrow(data)]

# Create a plotting data frame for convenience
plot_data_linear <- data.frame(
  Fecha = as.Date(test_dates),
  Actual = test_actual,
  Predicted = model_linear_predictions
)

# Plot Linear Regression Model
ggplot(plot_data_linear, aes(x = Fecha)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.5) +
  geom_line(aes(y = Predicted, color = "Predicted"), linewidth = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "lightblue", "Predicted" = "firebrick")) +
  labs(
    title = "Actual vs Predicted Caudal (Test Set)",
    x = "Date (Fecha)",
    y = "Caudal",
    color = "Legend"
  ) +
  theme_minimal()


# Ridge, Lasso and Elastic Net Section

# Create data matrices for the models
data_features <- subset(data, select=-Caudal)
target_feature_vec <- data$Caudal

# Matrices
X_train <- as.matrix(data_features[1:training_size, ])
X_test  <- as.matrix(data_features[(training_size + 1):n, ])
y_train <- target_feature_vec[1:training_size]
y_test  <- target_feature_vec[(training_size + 1):n]

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
  scale_color_manual(values=c("Actual"='lightblue', "Predicted"='firebrick'))+
  labs(
    title="Actual vs Predicted Caudal (Ridge Model)",
    x='Date(Fecha)',
    y='Caudal',
    color='Legend'
  )+
  theme_minimal()

rmse_ridge <- RMSE(model_ridge_predictions, y_test)
nse <- NSE(y_test, model_ridge_predictions)

rmse_ridge
nse

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
  scale_color_manual(values=c("Actual"='lightblue', "Predicted"='firebrick'))+
  labs(
    title="Actual vs Predicted Caudal (Lasso Model)",
    x='Date(Fecha)',
    y='Caudal',
    color='Legend'
  )+
  theme_minimal()

rmse_lasso <- RMSE(model_lasso_predictions, y_test)
rmse_lasso

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
  scale_color_manual(values=c("Actual"='lightblue', "Predicted"='firebrick'))+
  labs(
    title="Actual vs Predicted Caudal (Elastic Net Model)",
    x='Date(Fecha)',
    y='Caudal',
    color='Legend'
  )+
  theme_minimal()

