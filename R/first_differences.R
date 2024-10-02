#' Specifying the number of first differences
#'
#' This function is used to specify that the time series will be preprocessed
#' using first differences.
#'
#' @param n An integer specifying the number of first differences to be applied.
#'   If the default (-1) is used, the number of first differences needed by the
#'   time series will be computed by the [forecast::ndiffs()] function.
#'
#' @return An integer with the number of first differences to be applied.
#' @export
#'
#' @examples
#' fd(1)
fd <- function(n = -1) {
  if (! (is.numeric(n) && length(n) == 1 && n >= -1 && floor(n) == n))
    stop("n parameter should be an integer scalar value >= -1")
  structure(n, class = "fd_preprocessing")
}

# first differences preprocessing
# timeS is the time series
# differences is the number of differences, if -1, the number of differences is
# estimated with the function forecast::ndiffs
fd_preprocessing <- function(timeS, differences = -1) {
  if (differences == -1) {
    differences <- forecast::ndiffs(timeS)
  }
  if (differences == 0) {
    prepro <- list(differences = 0)
    return(structure(prepro, class = "fd"))
  }
  prepro <- list()
  prepro$original <- timeS
  
  last_values <- utils::tail(timeS, differences)
  timeS <- diff(timeS, differences = differences)
  prepro$last_values <- last_values
  prepro$differences <- differences
  
  prepro$preprocessed <- timeS
  structure(prepro, class = "fd")
}

# first differences unpreprocessing
# forecast: a forecast based on the first differenced series
# prepro: information about the first differences preprocessing
fd_unpreprocessing <- function(forecast, prepro) {
  temp <- stats::diffinv(forecast, differences = prepro$differences, xi = prepro$last_values)
  forecast <- subset(temp, start = 1 + prepro$differences)
  return(forecast)
}
