refit_mlm <- function(model, train_data, test_data) {
  refit_mlm_X <<- train_data[, -1]
  refit_mlm_y <<- train_data[, 1]
  refit_mlm_test <<- test_data
  prediction_mlm <- vector("list", length = length(model$models))
  modelnames <- row.names(model$pred_accuracy)
  names(prediction_mlm) <- modelnames
  for (i in seq_len(length(model$models))) {
    refit_mlm_temp <<- model$models[[i]]
    reticulate::source_python("R/scratch/refit.py")
    prediction_mlm[[i]] <- predictions
  }
  prediction_mlm
}
# Just return predictions
