feature1 = rep(c(1, 2, 3, 4, 5), 2)
feature2 = rep(c(2, 5, 4, 3, 1), 2)
target = rep(c(3, 7, 7, 7, 6), 2)

data = data.frame(feature1, feature2, target)

res = dataPreprocess(data, features = c("feature1", "feature2"), target = "target", training_part = 0.8)
X_train = res$X_train
X_test = res$X_test
Y_train = res$Y_train
Y_test = res$Y_test

seq_1 = c(1, 1, 1, 1, 1, 1, 1, 1)
feature1 = c(1, 2, 2, 3, 4, 5, 1, 4)
feature2 = c(2, 5, 5, 4, 3, 1, 2, 3)
X_train = matrix(c(seq_1, feature1, feature2), 8, 3)
colnames(X_train) = list("seq_1", "feature1", "feature2")

seq_1 = c(1, 1)
feature1 = c(3, 5)
feature2 = c(4, 1)
X_test = matrix(c(seq_1, feature1, feature2), 2, 3)
colnames(X_test) = list("seq_1", "feature1", "feature2")

Y_train = c(3, 7, 7, 7, 7, 6, 3, 7)
Y_test = c(7, 6)

res = list(X_train = X_train, X_test = X_test, Y_train = Y_train, Y_test = Y_test)
test_that("multiplication works", {
  expect_equal(dataPreprocess(data, features = c("feature1", "feature2"), target = "target", training_part = 0.8), res)
})
