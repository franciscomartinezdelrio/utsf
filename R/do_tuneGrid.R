#'Estimate the forecast accuracy of a model on a time series according to a grid
#'of parameters
#'
#'It uses an object of class `utsf` to asses the forecasting accuracy of its
#'associated model on its associated time series applying rolling origin
#'evaluation according to different configurations of model parameters.
#'
#'The estimation of forecast accuracy is done with the [efa()] function. The
#'best combination of parameters is used to train the model with all the
#'historical values of the time series and forecast `h` values ahead.
#'@inheritParams efa
#'@param tuneGrid A data frame with possible tuning values. The columns are
#'  named as the tuning parameters.
#'@returns A list with three components: \item{`tuneGrid`}{A data frame with the
#'  different combination of parameters and the estimated forecast accuracy of a
#'  model trained with those parameters.} \item{`best`}{The best combination of
#'  parameters according to root mean squared error.} \item{`forecast`}{An
#'  object of class `utsf_forecast` with the forecast for horizon `h` using the
#'  best estimated combination of parameters.}
#'@export
#'
#' @examples
#' m <- create_model(UKgas, lags = 1:4, method = "knn")
#' tune_grid(m, h = 4, tuneGrid = expand.grid(k = 1:7), type = "normal", size = 8)
tune_grid <- function(model, h, tuneGrid, type = c("normal", "minimum"), size = NULL, prop = NULL) {
  # Check model parameter
  if (!inherits(model, "utsf"))
    stop("model parameter should be of class 'utsf'")

    # Check h parameter
  if (! (is.numeric(h) && length(h) == 1 && h >= 1 && floor(h) == h))
    stop("h parameter should be an integer scalar greater than zero")

  # Check tuneGrid parameter
  if (! is.data.frame(tuneGrid))
    stop("parameter tuneGrid should be a data frame")
  
  # Check type parameter
  type <- match.arg(type)
  if (type == "normal" && !is.null(size) && !is.null(prop))
    stop("Only one of size and prop parameters should be different from NULL")
  # Check size parameter
  if (!is.null(size) && (length(size) > 1 || !is.numeric(size) || round(size) != size || size <= 0))
    stop("size parameter should be a positive integer scalar")
  # Check prop parameter
  if (!is.null(prop) && (length(prop) > 1 || !is.numeric(prop) || prop <= 0 || prop >= 1))
    stop("prop parameter should be a number in the range (0, 1)")
  
  if (length(model$ts) <= h)
    stop("Time series is too short to estimate forecast accuracy")

  l <- as.list(model$call)[-1]
  l$param <- NULL
  output <- NULL
  for (r in 1:nrow(tuneGrid)) {
    l$param <- as.list(tuneGrid[r, , drop = FALSE])
    m <- do.call("create_model", args = l)
    r <- efa(m, h = h, type = type, size = size, prop = prop)
    output <- rbind(output, r$global)
  }
  output <- cbind(tuneGrid, output)
  rownames(output) <- NULL
  best <- as.list(output[which.min(output$RMSE), 1:ncol(tuneGrid), drop = FALSE])
  l$param <- best
  m <- do.call("create_model", args = l)
  f <- forecast(m, h = h)
  list(tuneGrid = output, best = best, forecast = f)
}
  