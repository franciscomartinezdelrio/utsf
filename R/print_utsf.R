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
  cat("Regression model: ")
  if (inherits(x$method, "function")) {
    cat("provided by user\n")
  } else {
    method <-  switch(x$method,
                      "knn" = "k-nearest neighbors",
                      "mt" = "model trees",
                      "rf" = "random forest",
                      "bagging" = "bagging"
    )
    cat(method, "\n")
  }
    
  if (is.null(x$efa) && is.null(x$tuneGrid)) {
    cat("Forecast:\n")
    print(x$pred)
  } else if (!is.null(x$efa)) {
    cat("Estimated average forecast accuracy for horizon ", length(x$pred), ":\n", sep = "")
    print(x$efa)
  }  else if (!is.null(x$tuneGrid)) {
    cat("Estimated average forecast accuracy for different combinations of tuning parameters:\n")
    print(x$tuneGrid)
  }
  invisible(x)
}
