test_that("timeS parameter", {
  expect_error(forecast("a", h = 12), "timeS parameter should be of class ts or a numeric vector")
  expect_no_error(forecast(1:36, h = 12))
  expect_no_error(forecast(AirPassengers, h = 12))
})

test_that("h parameter", {
  expect_error(forecast(AirPassengers, h = "a"), "h parameter should be an integer scalar value >= 1")
  expect_error(forecast(AirPassengers, h = 1:2), "h parameter should be an integer scalar value >= 1")
})

test_that("lags parameter", {
  expect_error(forecast(AirPassengers, h = 12, lags = "a"), "lags parameter should be NULL or numeric")
  expect_error(forecast(AirPassengers, h = 12, lags = 3:1), "lags should be a vector in increasing order")
})
