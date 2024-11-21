build_model <- function(X, y, method, param) {
  if (method == "knn") {
    k <- if ("k" %in% names(param)) param[["k"]] else 3
    model <- paste0("K-nearest neighbors with k = ", k)
  } else if (method == "rt") {
    df <- cbind(X, targets = y)
    args <- list(formula = targets ~ .,
                 data = df,
                 method = "anova")
    args <- c(args, param)
    model <- do.call(rpart::rpart, args = args)
  } else if (method == "mt") {
    args <- list(x = as.data.frame(X), y = y)
    args <- c(args, param)
    model <- do.call(Cubist::cubist, args = args)
  }  else if (method == "bagging") {
    df <- cbind(X, targets = y)
    args <- list(formula = targets ~ ., data = df)
    args <- c(args, param)
    model <- do.call(ipred::bagging, args = args)
  } else if (method == "rf") { # random forest
    df <- cbind(X, targets = y)
    args <- list(formula = targets ~ .,
                 data = df,
                 mtry = floor((ncol(df)-1)/3)
    )
    args <- args[!(names(args) %in% names(param))]
    args <- c(args, param)
    model <- do.call(ranger::ranger, args = args)
  }
  model
}

#' Prediction from `utsf` objects
#'
#' Predict the class of a new observation based on the model associated with the
#' `utsf` object
#' @param object object of class `utsf`.
#' @param new_value a data frame with one row of a new observation.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric value with the forecast.
#' @export
predict.utsf <- function(object, new_value, ...) {
  if (object$method == "knn") {
    check_param(object, FNN::knn.reg, "FNN::knn.reg")
    args <- list(train = object$features,
                 test = new_value,
                 y = object$targets)
    args <- c(args, object$param)
    return(do.call(FNN::knn.reg, args = args)$pred)
  } else if (object$method == "rf") {
    return(stats::predict(object$model, new_value)$predictions)
  } else {
    return(stats::predict(object$model, new_value))
  }
}

# Check that parameters provided by the user for customizing model building belong 
# to building function. If not, execution is stopped
# param object An object of class utsf
# param f An object of class function. The function 
# param fname A string. The name of the function
check_param <- function(object, f, fname) {
  formal <- methods::formalArgs(f)
  dif <- setdiff (names(object$param), formal)
  if (length(dif) > 0)
    stop(paste("Error in \"param\" argument, parameters", dif, "are not part of",
               fname, "function"))
}
