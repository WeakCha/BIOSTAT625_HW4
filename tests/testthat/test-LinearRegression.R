feature1 = rep(c(1, 2, 3, 4, 5), 2)
feature2 = rep(c(2, 5, 4, 3, 1), 2)
target = rep(c(3, 7, 7, 7, 6), 2)

data = data.frame(feature1, feature2, target)

pred = matrix(c(7, 6), 2, 1)
RMSE = 0
res = list(pred = pred, RMSE = RMSE)
test_that("LinearRegression works", {
  expect_equal(LinearRegression(data, training_part = 0.8, features = c("feature1", "feature2"), target = "target", seed = 200), res)
})
