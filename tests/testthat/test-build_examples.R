test_that("Build examples", {
  features <- matrix(c(1:3, 2:4), nrow = 3)
  colnames(features) <- c("Lag2", "Lag1")
  r <- list(features = features, targets = 3:5)
  expect_equal(build_examples(ts(1:5), lags = 2:1), r)
})

