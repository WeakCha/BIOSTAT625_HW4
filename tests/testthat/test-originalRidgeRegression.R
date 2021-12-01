feature1 = rep(c(1, 2, 3, 4, 5), 2)
feature2 = rep(c(2, 5, 4, 3, 1), 2)
target = rep(c(3, 7, 7, 7, 6), 2)

data = data.frame(feature1, feature2, target)

pred = matrix(round(c(7, 6), 2), 2, 1)
RMSE = 0
res = list(pred = pred, RMSE = RMSE)

true = originalRidgeRegression(data, features = c("feature1", "feature2"), target = "target", training_part = 0.8, seed = 200)
true$pred = round(true$pred, 2)
test_that("originalRidgeRegression works", {
  expect_equal(true, res)
})
