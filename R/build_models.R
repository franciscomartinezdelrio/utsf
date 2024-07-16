build_model <- function(X, y, method, param) {
  if (method == "knn") {
    return(NULL)
  } 
  if (method == "rt") {
    df <- cbind(X, targets = y)
    args <- list(formula = targets ~ .,
                 data = df,
                 method = "anova")
    args <- c(args, param)
    return(do.call(rpart::rpart, args = args))
  } 
  if (method == "mt") {
    args <- list(x = as.data.frame(X), y = y)
    args <- c(args, param)
    return(do.call(Cubist::cubist, args = args))
  } 
  if (method == "bagging") {
    df <- cbind(X, targets = y)
    args <- list(formula = targets ~ ., data = df)
    args <- c(args, param)
    return(do.call(ipred::bagging, args = args))
  } 
  if (method == "rf") { # random forest
    df <- cbind(X, targets = y)
    args <- list(formula = targets ~ .,
                 data = df,
                 mtry = floor((ncol(df)-1)/3)
    )
    args <- args[!(names(args) %in% names(param))]
    args <- c(args, param)
    return(do.call(ranger::ranger, args = args))
  }
}

# How to predict next future value with knn
# param model An object of class utsf
predict_one_value_knn <- function(model, example) {
  check_param(model, FNN::knn.reg, "FNN::knn.reg")
  args <- list(train = model$features,
               test = example,
               y = model$targets)
  args <- c(args, model$param)
  do.call(FNN::knn.reg, args = args)$pred
}

# How to predict next future value (just one value) with regression trees
# param model An object of class utsf
predict_one_value_rt <- function(model, example) {
  stats::predict(model$model, example)
}

# How to predict next future value with random forest
# param model An object of class utsf
predict_one_value_rf <- function(model, example) {
  stats::predict(model$model, example)$predictions
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
