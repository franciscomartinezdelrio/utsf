test_that("timeS parameter", {
  expect_error(create_model("a"), "timeS parameter should be of class ts or a numeric vector")
  expect_no_error(create_model(1:36, lags = 1:2))
  expect_no_error(create_model(AirPassengers))
})

test_that("lags parameter", {
  expect_error(create_model(AirPassengers, lags = "a"), "lags parameter should be NULL or numeric")
  expect_error(create_model(AirPassengers, lags = 1:3 + 0.3), "lag values should be integer")
  expect_error(create_model(AirPassengers, lags = -1:3), "lag values should be greater than zero")
  expect_equal(create_model(AirPassengers, lags = 3:1)$lags, 1:3)
})

test_that("h parameter", {
  expect_error({
      m <- create_model(AirPassengers)
      forecast(m, h = "a")
    }, "h parameter should be an integer scalar greater than zero")
  expect_error({
    m <- create_model(AirPassengers)
    forecast(m, h = 1:12)
  }, "h parameter should be an integer scalar greater than zero")
})

test_that("timeS parameter", {
  expect_error(create_model("a"), "timeS parameter should be of class ts or a numeric vector")
  expect_no_error(create_model(1:36, lags = 1:2))
  expect_no_error(create_model(AirPassengers))
})

test_that("h parameter", {
  expect_error({
    m <- create_model(AirPassengers)
    forecast(m, h = "a")
    }, "h parameter should be an integer scalar greater than zero")
  expect_error({
    m <- create_model(AirPassengers)
    forecast(m, h = 1:12)
    }, "h parameter should be an integer scalar greater than zero")
})

test_that("lags parameter", {
  expect_error(create_model(AirPassengers, lags = "a"), "lags parameter should be NULL or numeric")
})


