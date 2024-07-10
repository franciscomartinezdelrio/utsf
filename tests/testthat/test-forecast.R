test_that("timeS parameter", {
  expect_error(forecast("a", h = 12), "timeS parameter should be of class ts or a numeric vector")
  expect_no_error(forecast(1:36, h = 12))
  expect_no_error(forecast(AirPassengers, h = 12))
})
