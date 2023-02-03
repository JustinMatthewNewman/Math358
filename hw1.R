# Use data Boston from ISLR2 pacakage. Treat crim (per capita crime rate)
# as the response variable and the rest as predictors.

#install.packages("ISLR2")
library("ISLR2")

# import data
data("Boston", package = "ISLR2")
Boston

# convert to data frame
data = data.frame(Boston)
data

# First choose 2/3 of the data as training data and 1/3 of the data as test
# data. Use the function in dplyr package to randomly split the data according
# to the (2/3, 1/3) proportion. 

# Use set.seed(1) before you split the data.
set.seed(1)

# Split the data into training (2/3) and test (1/3) data
#install.packages("dplyr")
library(dplyr)

n=nrow(Boston)
train = Boston %>%
  sample_n(0.67*n)
test = Boston %>%
  setdiff(train)             # randomly spilt data into two parts
train
test


# USE PREDICT FUNCTION 

# a). Use the training data, for each predictor, fit a simple linear regression
# model to predict the response. Using α = 0.05, which predictors have a statis-
#   tically significant association with the response?

# Part a: Fit simple linear regression models for each predictor

# Create an empty vector to store p-values
p_values <- numeric(length(names(train)) - 1)

# Loop over each predictor
for (i in 2:length(names(train))) {
  
  # Fit a simple linear regression model
  model <- lm(train[, 1] ~ train[, i])
  
  # Extract the p-value for the predictor
  p_values[i - 1] <- summary(model)$coef[2, 4]
}

# Find the predictors with significant p-values
significant_predictors_a <- names(train)[2:length(names(train))][p_values < 0.05]
significant_predictors_a


# b). Use the traning data only, fit a multiple linear regression using all the
# predictors. Choose the significant predictors from the output 
# (i.e., choose the  predictors with coefficient H0: βj= 0 is rejected at α = 0.05).

# Fit the multiple linear regression model
fit <- lm(crim ~ ., data = train)
# Summarize the model
summary(fit)

# Extract the p-values for each predictor
pvals <- summary(fit)$coefficients[,4]

# Select predictors with p-values less than 0.05
significant_predictors_b <- names(which(pvals < 0.05))
significant_predictors_b

# c). Fit a multiple regression model using the selected predictors from a) and b) 
# respectively based on the training data. Use the fitted model to predict the
# response in the test data respectively. Compare the test error produced by the
# two models. Which model performs better based on the test error?


#Fit multiple regression model using the selected predictors from part a
model_a <- lm(crim ~ ., data = train[, c("crim", significant_predictors_a)])

#Predict the response in the test data using the model from part a
pred_a <- predict(model_a, newdata = train[, c("crim", significant_predictors_a)])

#Calculate the mean squared error for the predictions from part a
mse_a <- mean((pred_a - test$crim)^2)



#Fit multiple regression model using the selected predictors from part b
model_b <- lm(crim ~ ., data = train[, c("crim", significant_predictors_b)])

#Predict the response in the test data using the model from part b
pred_b <- predict(model_b, newdata = train[, c("crim", significant_predictors_b)])

#Calculate the mean squared error for the predictions from part b
mse_b <- mean((pred_b - test$crim)^2)

mse_a
mse_b

#Compare the mean squared errors for the two models
if (mse_a < mse_b) {
  cat("Model from part a performs better on the test data, with a mean squared error of", mse_a)
} else {
  cat("Model from part b performs better on the test data, with a mean squared error of", mse_b)
}

