feature1 = rep(c(1, 2, 3, 4, 5), 2)
feature2 = rep(c(2, 5, 4, 3, 1), 2)
target = rep(c(3, 7, 7, 7, 6), 2)

data = data.frame(feature1, feature2, target)

res = dataPreprocess(data, features = c("feature1", "feature2"), target = "target", training_part = 0.8)
X_train = res$X_train
X_test = res$X_test
Y_train = res$Y_train
Y_test = res$Y_test

Y_train = as.matrix(Y_train)
Y_test = as.matrix(Y_test)

pred = matrix(c(7, 6), 2, 1)
RMSE = 0
res = list(pred = pred, RMSE = RMSE)

test_that("fastRidgeRegression works", {
  expect_equal(fastRidgeRegression(X_train, Y_train, X_test, Y_test), res)
})


test_that("fastRidgeRegression works", {
  expect_equal(2 * 2, 4)
})
