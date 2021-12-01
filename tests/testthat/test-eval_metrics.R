true = c(1, 2)
pred = c(1, 2)

RMSE = 0
test_that("eval_metrics works", {
  expect_equal(eval_metrics(true, pred), 0)
})
