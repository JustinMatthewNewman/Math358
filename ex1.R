library(ISLR2)
library(glmnet)
library(dplyr)
library(tidyr)

set.seed(1)

n=nrow(Boston)

train = Boston %>%
  sample_n(0.67*n)

test = Boston %>%
  setdiff(train)           

train=na.omit(train)
x = model.matrix(crim~., train)[,-1]
y = train$crim
cv.out <- cv.glmnet(x,y,alpha=0,nfolds=5)
print(cv.out$cvm)
plot(cv.out)

bestlam=cv.out$lambda.min
bestlam

ridge.out=glmnet(x,y,alpha=0,lambda=bestlam)
ridge.out$beta [ridge.out$beta != 0]

# the best lamda to fit the ridge regression model using train data

# fit the ridge regression model with the best lambda
fit = glmnet(x, y, alpha=0, lambda=bestlam)

# make predictions on the training data
train.pred = predict(fit, newx=x)

# calculate the mean squared error on the training data
train.mse = mean((y - train.pred)^2)
train.mse

# Use the output to predict the y for the test data

test.x = model.matrix(crim~., data=test)
test.pred = predict(fit, newx=test.x[,-1])

#calculate the mean squared error on the test data
test.mse = mean((test$crim - test.pred)^2)
test.mse

