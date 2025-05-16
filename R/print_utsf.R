#' @export
print.utsf <- function (x, ...) {
  cat("\nCall:  ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"
      ),
      "\n\n",
      sep = ""
  )
  cat("Autoregressive lags:", x$lags, "\n")
  if (what_preprocess(x$preProcess) %in% c("additive", "multiplicative")) {
    cat (what_preprocess(x$preProcess), "tranformation applied.\n")
  }
  if (what_preprocess(x$preProcess) == "differences") {
    cat("First differences applied as preprocessing.", nd2character(x$differences), "\n")
  }
  cat("Regression model: ")
  if (inherits(x$method, "function")) {
    cat("provided by user\n")
  } else {
    method <-  switch(x$method,
                      "knn" = "k-nearest neighbors",
                      "lm" = "linear model",
                      "rt" = "regression trees",
                      "mt" = "model trees",
                      "rf" = "random forest",
                      "bagging" = "bagging"
    )
    cat(method, "\n")
  }
    
  if (is.null(x$global_efa) && is.null(x$tuneGrid)) {
    cat("Forecast:\n")
    print(x$pred)
  } else if (!is.null(x$global_efa)) {
    cat("Estimated average forecast accuracy for horizon ", length(x$pred), ":\n", sep = "")
    print(x$global_efa)
  }  else if (!is.null(x$tuneGrid)) {
    cat("Estimated average forecast accuracy for different combinations of tuning parameters:\n")
    print(x$tuneGrid)
    minimum <- which.min(x$tuneGrid$RMSE)
    cat("\nBest combination according to RMSE:\n")
    print(x$tuneGrid[minimum, ])
  }
  invisible(x)
}

#' @export
summary.utsf <- function (object, ...) structure(object, class = "summary.utsf")

#' @export
print.summary.utsf <- function (x, ...) {
  cat("\nCall:  ",
      paste(deparse(x$call),
            sep = "\n",
            collapse = "\n"
      ),
      "\n\n",
      sep = ""
  )
  cat("Autoregressive lags:", x$lags, "\n")
  if (what_preprocess(x$preProcess) %in% c("additive", "multiplicative")) {
    cat (x$preProcess[[1]], "tranformation applied\n")
  }
  if (what_preprocess(x$preProcess) == "fd") {
    cat("First differences applied as preprocessing.", nd2character(x$fd), "\n")
  }
  cat("Regression model: ")
  if (inherits(x$method, "function")) {
    cat("provided by user\n")
  } else {
    method <-  switch(x$method,
                      "knn" = "k-nearest neighbors",
                      "lm" = "linear model",
                      "rt" = "regression trees",
                      "mt" = "model trees",
                      "rf" = "random forest",
                      "bagging" = "bagging"
    )
    cat(method, "\n")
  }
  invisible(x)
}

