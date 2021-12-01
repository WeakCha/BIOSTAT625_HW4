#data <- read.csv("data/heart.csv")

install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("lattice", repos = "http://cran.us.r-project.org")
install.packages("Matrix", repos = "http://cran.us.r-project.org")
install.packages("survival", repos = "http://cran.us.r-project.org")
install.packages("glmnet", repos = "http://cran.us.r-project.org")

library(dplyr)
library(lattice)
library(Matrix)
library(glmnet)
library(survival)
#flag = require("dplyr")
#if(flag == FALSE){
#  install.packages("dplyr")
#  library(dplyr)
#}

#flag = require("survival")
#if(flag == FALSE){
#  install.packages("survival", repos = "http://cran.us.r-project.org")
#  library(survival)
#}

#flag = require("lattice")
#if(flag == FALSE){
#  install.packages("lattice", repos = "http://cran.us.r-project.org")
#  library(lattice)
#}

#flag = require("Matrix")
#if(flag == FALSE){
#  install.packages("Matrix", repos = "http://cran.us.r-project.org")
#  library(Matrix)
#}

#flag = require("glmnet")
#if(flag == FALSE){
#  install.packages("glmnet", repos = "http://cran.us.r-project.org")
#  library(glmnet)
#}



dataPreprocess = function(data, features, target, training_part = 0.8){
  X = select(data, features)
  Y = select(data, target)
  X = as.matrix(X)
  Y = as.matrix(Y)
  X_row = nrow(X)
  seq_1 = rep(1, X_row)
  X = cbind(seq_1, X)

  index = sample(1:nrow(data), training_part*nrow(data))

  X_train = X[index,] # Create the training data
  X_test = X[-index,] # Create the test data
  Y_train = Y[index,]
  Y_test = Y[-index,]

  return(list(X_train = X_train, X_test = X_test, Y_train = Y_train, Y_test = Y_test))
}

originalRidgeRegression = function(data, features = NULL, target, training_part = 0.8, seed = 200){
  # Model requiring glmnet
  #flag = require("glmnet")
  #if(flag == FALSE){
  #  install.packages("glmnet")
  #  library(glmnet)
  #}

  set.seed(seed)
  # res = dataPreprocess(data, features, target, training_part)
  # X_train = res$X_train
  # X_test = res$X_test
  # Y_train = res$Y_train
  # Y_test = res$Y_test

  index = sample(1:nrow(data), training_part*nrow(data))

  data_train = data[index,]
  data_test = data[-index,]

  X_train = data.matrix(data_train[, features])
  Y_train = data_train[, target]
  #cols_reg = c(features, target)
  #dummies <- dummyVars(target ~ ., data = data_train[,cols_reg])
  #train_dummies = predict(dummies, newdata = data_train[,cols_reg])

  #X_train = as.matrix(train_dummies)
  #Y_train = data_train$target

  #dummies <- dummyVars(target ~ ., data = data_test[,cols_reg])
  #test_dummies = predict(dummies, newdata = data_test[,cols_reg])

  X_test = data.matrix(data_test[, features])
  Y_test = data_test[, target]

  # print(X_train)

  model = glmnet(X_train, Y_train, alpha = 0, lambda = 0, family = "gaussian")
  # print(model$beta)
  pred = predict(model, X_test)
  pred = as.vector(pred)
  RMSE = eval_metrics(Y_test, as.vector(pred))
  pred = as.matrix(pred)
  return (list(pred = pred, RMSE = RMSE))
}

fastRidgeRegression = function(X_train, Y_train, X_test, Y_test, training_part = 0.8){
  res_svd = svd(X_train)
  tuy = crossprod(res_svd$u, Y_train)
  beta = res_svd$v %*% (tuy * res_svd$d / (res_svd$d^2 ))
  # print(beta)

  pred = crossprod(t(X_test), beta)

  RMSE = eval_metrics(Y_test, pred)
  return (list(pred = pred, RMSE = RMSE))
}

eval_metrics = function(true, predictions){
  resids = true - predictions
  resids2 = resids**2
  N = length(predictions)
  RMSE = round(sqrt(sum(resids2)/N), 2)
  return(RMSE)
}

LinearRegression = function(data, training_part = 0.8, features = NULL, target, seed = 200){
  #flag = require("dplyr")
  #if(flag == FALSE){
  #  install.packages("dplyr")
  #}
  #library("dplyr")
  set.seed(seed)
  res = dataPreprocess(data, features, target, training_part)
  X_train = res$X_train
  X_test = res$X_test
  Y_train = res$Y_train
  Y_test = res$Y_test

  Y_train = as.matrix(Y_train)
  Y_test = as.matrix(Y_test)

  res = fastRidgeRegression(X_train, Y_train, X_test, Y_test, training_part)
  beta = res$beta
  pred = res$pred
  RMSE = res$RMSE
  return (list(pred = pred, RMSE = RMSE))
}

#flag = require(bench)
#if(flag == FALSE){
#  install.packages("bench")
#}

#flag = require(gmp)
#if(flag == FALSE){
#  install.packages("gmp")
#}

#flag = require(usethis)
#if(flag == FALSE){
#  install.packages("usethis")
#}

#library(bench)
#library(gmp)
#LinearRegression(data, features = c("sex", "cp"), target = "target", 0.8, seed = 300)
#bench::mark(originalRidgeRegression(data, features = c("sex", "cp"), target = "target", 0.8), LinearRegression(data, features = c("sex", "cp"), target = "target", 0.8))
