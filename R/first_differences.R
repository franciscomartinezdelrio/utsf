#' Specifying the order of differences
#'
#' This function is used to specify that the time series will be preprocessed
#' using first differences.
#'
#' @param n An integer specifying the order of differences to be applied.
#'   If the default (-1) is used, the order of differences needed by the
#'   time series will be computed by the [forecast::ndiffs()] function.
#'
#' @return An integer with the order of differences to be applied.
#' @export
#'
#' @examples
#' differences(1)
differences <- function(n = -1) {
  if (! (is.numeric(n) && length(n) == 1 && n >= -1 && floor(n) == n))
    stop("n parameter should be an integer scalar value >= -1")
  structure(n, class = "fd_preprocessing")
}

# Differences preprocessing
# timeS is the time series
# differences is the number of differences, if -1, the number of differences is
# estimated with the function forecast::ndiffs
fd_preprocessing <- function(timeS, differences = -1) {
  prepro <- list()
  prepro$asked_differences <- differences
  if (differences == -1) {
    differences <- forecast::ndiffs(timeS)
  }
  if (differences == 0) {
    prepro$differences <- 0
    return(structure(prepro, class = "fd"))
  }
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
  utils::tail(temp, -prepro$differences)
}

nd2character <- function(p) {
  o <- "Number of differences"
  if (p$asked_differences == -1) o <- paste(o, "(selected automatically)")
  paste0(o, ": ", p$differences)
}
#' @export
print.fd <- function(x, ...) {
  cat(nd2character(x), "\n")
  invisible(x)
}
