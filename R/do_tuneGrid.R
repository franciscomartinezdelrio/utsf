do_tuneGrid <- function(timeS, h, lags, method, tuneGrid, transform, type) {
  if (length(timeS) <= h) {
    stop("Time series is too short to do search grid")
  }
  type <- if(is.null(type)) "fixed" else type
  output <- NULL
  for (r in 1:nrow(tuneGrid)) {
    result <- estimate_accuracy(timeS = timeS,
                                h = h,
                                lags = lags,
                                method = method,
                                param = as.list(tuneGrid[r, ]),
                                transform = transform,
                                type = type
    )
    output <- rbind(output, result)
  }
  output <- cbind(tuneGrid, output)
  rownames(output) <- NULL
  output
} 