#' Create a ggplot object from an `utsf_forecast` object
#'
#' Plot the time series and its associated forecast.
#'
#' @param object An object of class `utsf_forecast`.
#' @param ... additional parameter.
#'
#' @return The `ggplot` object representing a plotting of the time series and
#'   its forecast.
#'
#' @examples
#' m <- create_model(AirPassengers, lags = 1:12, method = "rf")
#' f <- forecast(m, h = 12)
#' library(ggplot2)
#' autoplot(f)
#' @export
#' @importFrom ggplot2 autoplot
autoplot.utsf_forecast <- function(object, ...) {
  if (is.null(object$lower)) {
    vctsfr::plot_ts(object$ts, prediction = object$pred)
  } else {
    vctsfr::plot_ts(object$ts, 
                    prediction = object$pred, 
                    lpi = object$lower,
                    upi = object$upper,
                    level = object$level
    )
  }
}

#' @importFrom graphics plot
#' @export
plot.utsf_forecast <- function(x, y, ...) {
  timeS <- stats::ts(c(x$ts, x$pred),
                     start = stats::start(x$ts),
                     frequency = stats::frequency(x$ts)
  )
  graphics::plot(timeS, type = "n", ylab = "")
  graphics::lines(x$ts, type = "o", pch = 20)
  graphics::lines(x$pred, type = "o", col = "#CC0000", pch = 20)
}
