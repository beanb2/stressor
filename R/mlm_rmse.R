mlm_rmse <- function(predictions, observed) {
  rmse <- vector("numeric", length = ncol(predictions))
  for (i in seq_len(ncol(predictions))) {
    rmse[i] <- sqrt(sum((observed - predictions[, i])^2) / nrow(predictions))
  }
  rmse
}
