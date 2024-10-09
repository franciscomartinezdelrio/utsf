#' Build the training examples
#'
#' Build the training examples for a regressive model to forecast a time series
#' using lagged values of the series as autoregressive features.
#'
#' @param timeS The time series.
#' @param lags An integer vector with the lags used as feature vector in
#'   decreasing order.
#'
#' @return A list with two fields: 1) a matrix with the features of the examples
#'   and 2) a vector with the targets of the examples
#' @export
#'
#' @examples
#' build_examples(ts(1:5), lags = 2:1)
build_examples <- function(timeS, lags) {
  MAXLAG   <- lags[1]
  NCOL     <- length(lags)
  NROW     <- length(timeS) - MAXLAG
  features <- matrix(0, nrow = NROW, ncol = NCOL)
  targets  <- vector(mode = "numeric", length = NROW)
  row <- 1
  for (ind in seq(MAXLAG + 1, length(timeS))) {
    features[row, ] <- timeS[ind - lags]
    targets[row] <- timeS[ind]
    row <- row + 1
  }
  colnames(features) <- paste0("Lag", lags)
  list(
    features = features,
    targets = targets
  )
}
