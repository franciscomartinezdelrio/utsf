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
  if (x$trend == "additive") {
    cat ("Additive tranformation applied to", 
         if (x$transform_features) "features and", "targets\n")
  }
  if (x$trend == "multiplicative") {
    cat ("Multiplicative tranformation applied to", 
         if (x$transform_features) "features and", "targets\n")
  }
  if (x$trend == "differences") {
    cat("First differences applied as preprocessing.", nd2character(x$differences), "\n")
  }
  cat("Regression model: ")
  if (inherits(x$method, "function")) {
    cat("provided by user\n")
  } else {
    method <-  switch(x$method,
                      "knn" = "k-nearest neighbors",
                      "lm" = "linear model",
                      "rt" = "regression tree",
                      "mt" = "model tree",
                      "rf" = "random forest",
                      "bagging" = "bagging"
    )
    cat(method, "\n")
  }
    
  # if (is.null(x$global_efa) && is.null(x$tuneGrid)) {
  #   cat("Forecast:\n")
  #   print(x$pred)
  # } else if (!is.null(x$global_efa)) {
  #   cat("Estimated average forecast accuracy for horizon ", length(x$pred), ":\n", sep = "")
  #   print(x$global_efa)
  # }  else if (!is.null(x$tuneGrid)) {
  #   cat("Estimated average forecast accuracy for different combinations of tuning parameters:\n")
  #   print(x$tuneGrid)
  #   minimum <- which.min(x$tuneGrid$RMSE)
  #   cat("\nBest combination according to RMSE:\n")
  #   print(x$tuneGrid[minimum, ])
  # }
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
  if (x$trend == "additive") {
    cat ("Additive tranformation applied to", 
         if (x$transform_features) "features and", "targets\n")
  }
  if (x$trend == "multiplicative") {
    cat ("Multiplicative tranformation applied to", 
         if (x$transform_features) "features and", "targets\n")
  }
  if (x$trend == "differences") {
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

#' @export
print.utsf_forecast <- function (x, ...) {
  if (is.null(x$lower)) {
    print(x$pred)
  } else {
    l = paste("Lo", x$level)
    h <- paste("Hi", x$level)
    o <- cbind(x$pred, l = x$lower, h = x$upper)
    colnames(o) <- c("Point Forecast", l, h)
    print(o)
  }
  invisible(x)
}

