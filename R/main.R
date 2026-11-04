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

# 2. Calculate clean train/test splits (80% / 20%)
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

# --- Linear Regression ---
model_linear <- lm(Caudal ~ ., data = training_set)
summary(model_linear)

model_linear_predictions <- predict(model_linear, newdata = test_set)

test_dates <- dates[(training_size + 1):nrow(data)]
test_actual <- data$Caudal[(training_size + 1):nrow(data)]

# create a plotting data frame for convenience
plot_data_linear <- data.frame(
  Fecha = as.Date(test_dates),
  Actual = test_actual,
  Predicted = model_linear_predictions
)

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


# ---Ridge, Lasso and Elastic Net---

# Create data matrices for the models
data_features <- subset(data, select=-Caudal)
target_feature_vec <- data$Caudal

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

plot_data_ridge <- data.frame(
  Fecha=as.Date(test_dates),
  Actual=test_actual,
  Predicted = as.vector(model_ridge_predictions)
)

ggplot(plot_data_ridge, aes(x=Fecha))+
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

# --- LASSSO ---


