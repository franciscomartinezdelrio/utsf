#' Specifying the transformation for dealing with trended series
#'
#' This function is used to specify the preprocessing associated with the trend
#' of a time series.
#'
#' @param type A character indicating the type of preprocessing applied to the
#'   time series. Possible values are: `"none"`, `"additive"`, `"multiplicative"`
#'   and `"differences"`.
#' @param n An integer specifying the order of first differences to be applied.
#'   If the default (-1) is used, the order of first differences needed by the
#'   time series will be estimated by the [forecast::ndiffs()] function. This
#'   parameter is only meant when the `type` parameter is `"differences"`.
#' @param transform_features A logical value indicating whether the training
#'   features are also transformed with the additive or multiplicative
#'   transformation.
#'
#' @return A list with the selected options
#' @export
#'
#' @examples
#' trend("none")            # no preprocessing
#' trend("additive")        # additive preprocessing
#' trend("differences", 1)  # order 1 first differences
#' trend("differences", -1) # order of first differences automatically estimated
trend <- function(type = "additive", n = -1, transform_features = FALSE) {
  if (! (is.character(type) && length(type) == 1))
    stop("type argument in trend_prepro should be a character")
  if (! type %in% c("none", "additive", "multiplicative", "differences"))
    stop("type argument in trend_prepro should be none, additive, multiplicative or differences")
  if (! (is.numeric(n) && length(n) == 1 && n >= -1 && floor(n) == n))
    stop("n parameter should be an integer scalar value >= -1")
  if (! (is.logical(transform_features) && length(transform_features) == 1))
    stop("transform_features parameter should be a logical value")
  structure(list(type = type, n = n, transform_features = transform_features), class = "trend")
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
  o <- "Order of first differences"
  if (p$asked_differences == -1) o <- paste(o, "(selected automatically)")
  paste0(o, ": ", p$differences)
}
#' @export
print.fd <- function(x, ...) {
  cat(nd2character(x), "\n")
  invisible(x)
}
