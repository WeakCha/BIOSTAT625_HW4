data <- read.csv("heart.csv")

# For inference
inference = function(data, features = NULL, target, model_name = "lm", family = "poisson"){
  # Sanity check
  if(!(model_name == "lm" || model_name == "glm" || model_name == "Ridge" || model_name == "LASSO" || model_name == "Elastic-Net")){
    stop("model name should be one of the following: lm, glm, Ridge, LASSO, Elastic-Net")
  }

  if(is.null(features)){
    # Default: all columns except target will be set as features
    features = names(data)[which(names(data) != target)]
  }

  if(model_name == "lm"){
    model = lm(as.formula(paste("target ~ ", paste(features, collapse = "+"))), data)
  }
  if(model_name == "glm"){
    model = glm(as.formula(paste("target ~ ", paste(features, collapse = "+"))), data, family = family)
  }

  # library check
  flag = require("caret")
  if(flag == FALSE){
    install.packages("caret")
    library(caret)
  }

  # Model requiring glmnet
  flag = require("glmnet")
  if(flag == FALSE){
    install.packages("glmnet")
    library(glmnet)
  }

  cols_reg = c(features, target)
  dummies <- dummyVars(target ~ ., data = data[,cols_reg])
  train_dummies = predict(dummies, newdata = data[,cols_reg])

  x_train = as.matrix(train_dummies)
  y_train = train$target

  if(model_name == "Ridge"){
    lambdas <- 10^seq(2, -3, by = -.1)
    cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
    optimal_lambda <- cv_ridge$lambda.min
    model = glmnet(x_train, y_train, alpha = 0, family = "gaussian", lambda = optimal_lambda)
  }

  if(model_name == "LASSO"){
    lambdas <- 10^seq(2, -3, by = -.1)
    cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
    optimal_lambda <- cv_ridge$lambda.min
    model <- glmnet(x_train, y_train, alpha = 1, lambda = 1, standardize = TRUE)
  }

  if(model_name == "Elastic-Net"){

    train_cont <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 5,
                               search = "random",
                               verboseIter = TRUE)

    model <- train(target ~ .,
                         data = data,
                         method = "glmnet",
                         preProcess = c("center", "scale"),
                         tuneLength = 10,
                         trControl = train_cont)
  }

  return(list(model = model, data = data))
}

# For training and evaluation

prediction = function(data, features = NULL, target, factors = NULL, model_name = "lm", family = "poisson", training_part = 0.8, preprocess = FALSE){
  # Sanity check
  stopifnot(is.vector(features) || is.null(factors))
  stopifnot(is.vector(target))
  stopifnot(is.vector(factors) || is.null(factors))
  stopifnot(is.null(family) || length(family) == 1)
  stopifnot(length(target) == 1)

  flag = require("dplyr")
  if(flag == FALSE){
    install.packages("dplyr")
  }
  X = select(data, c(features, target))

  Y = select(data, target)
  number = nrow(unique(Y))
  if(number == 1){
    stop("the number of unique values of target is 1, stopped!")
  }
  if(number == 2){
    family = "binomial"
  }
  if(!is.null(factors)){
    for(i in factors){
      X[, i] = factor(X[, i])
    }
  }

  index = sample(1:nrow(data), training_part*nrow(data))

  train = X[index,] # Create the training data
  test = X[-index,] # Create the test data

  if(preprocess){
    cols = setdiff(features, factors)
    pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))
    train[,cols] = predict(pre_proc_val, train[,cols])
    test[,cols] = predict(pre_proc_val, test[,cols])
  }

  res = inference(train, features = features, target = target, model_name = model_name, family = family)
  model = res$model

  cols_reg = c(features, target)
  dummies <- dummyVars(target ~ ., data = test[,cols_reg])
  test_dummies = predict(dummies, newdata = test[,cols_reg])

  x_test = as.matrix(test_dummies)
  y_test = test$target

  print(summary(model))
  print(x_test)

  result = predict(model, x_test)
  #eval = eval_metrics(model, model_name, test, result, target)
  #return(list(result = result, model = model, eval = eval))
  return(list(result = result, model = model))
}

#Step 1 - create the evaluation metrics function

#eval metrics used for continuous response
eval_metrics = function(model, model_name, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  RMSE = round(sqrt(sum(resids2)/N), 2)
  if(model_name == "lm"){
    r2 = as.character(round(summary(model)$r.squared, 2))
    adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
    return(list(r2 = r2, adj_r2 = adj_r2, RMSE = RMSE))
  }
  if(model_name == "glm"){
    AIC = model$aic
    return(list(AIC = AIC, RMSE = RMSE))
  }
}

# Step 2 - predicting and evaluating the model on train data
# predictions = predict(lr, newdata = train)
# eval_metrics(lr, train, predictions, target = 'target')

# Step 3 - predicting and evaluating the model on test data
# predictions = predict(lr, newdata = test)
# eval_metrics(lr, test, predictions, target = 'target')
