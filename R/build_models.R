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