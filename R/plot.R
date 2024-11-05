#' Create a ggplot object from an `utsf` object
#'
#' Plot the time series and its associated forecast.
#'
#' @param object An object of class `utsf`.
#' @param ... additional parameter.
#'
#' @return The `ggplot` object representing a plotting of the time series and
#'   its forecast.
#'
#' @examples
#' f <- forecast(AirPassengers, h = 12, lags = 1:12, method = "rf")
#' library(ggplot2)
#' autoplot(f)
#' @export
#' @importFrom ggplot2 autoplot
autoplot.utsf <- function(object, ...) {
  vctsfr::plot_ts(object$ts, prediction = object$pred)
}

#' @importFrom graphics plot
#' @export
plot.utsf <- function(x, y, ...) {
  timeS <- stats::ts(c(x$ts, x$pred),
                     start = stats::start(x$ts),
                     frequency = stats::frequency(x$ts)
  )
  graphics::plot(timeS, type = "n", ylab = "")
  graphics::lines(x$ts, type = "o", pch = 20)
  graphics::lines(x$pred, type = "o", col = "#CC0000", pch = 20)
}
