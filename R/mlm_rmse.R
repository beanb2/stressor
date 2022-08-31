mlm_rmse <- function(predictions, response) {
  n_models <- length(predictions[[1]])
  pred_accuracy <- matrix(0, ncol = length(predictions), nrow = n_models)
  for (i in seq_len(length(predictions))) {
    pred_accuracy[, i] <- accuracy(predictions[[i]], response)
  }
  pred_accuracy
}
