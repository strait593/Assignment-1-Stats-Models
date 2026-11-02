library(ggplot2)
library(glmnet)
library(MLmetrics)

df <- read.csv("data/jardin_data.csv")
df <- as.data.frame(df)

# Get an understanding on what we are dealing with
head(df)
summary(df)
length(colnames(df))

train_df <- subset(df, select = -c(Fecha, Caudal))
test_df <- df$Caudal

# Linear model requires data frame object
nrow(df)
nrow(df) * 0.8

# gonna have to hard-code this...
training_set <- train_df[1:(0.8 * nrow(train_df)), ]
testing_set <- test_df[9662:12077,]

# Create an instance of Linear Model
model_linear <- lm(df$Caudal~ ., data=training_set)

# convert into matrix for glmnet to work
X <- as.matrix(subset(df, select=-c(Fecha, Caudal)))
y <- df$Caudal

# split the data into X_train, X_test, y_train, y_test

# Create an instance of the linear model
model_linear <- lm(target_for_linear_model~. , data=df_for_linear_model)
summary(model_linear)

model_linear_predictions

ggplot(data=df_for_linear_model, aes(y=target_for_linear_model))+
  geom_line(aes(model_linear), col="firebrick", alpha=0.7)+
  theme_classic()