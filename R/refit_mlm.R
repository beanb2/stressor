#' Refit Machine Learning Models
#'
#' Refits models fitted in the [compare_mlm()], and returns the predictions.
#' @param models The returned object of the [compare_mlm()]
#' @param train_data A dataframe object used for refitting excludes the test
#'   data, making sure that the response variable is first.
#' @param test_data A dataframe object used for predictions, making sure that
#'   the response variable is first.
#' @return A named list object of result from pycaret's [predict_model()].
refit_mlm <- function(models, train_data, test_data, classification = FALSE) {
  refit_mlm_X <<- train_data[, -1]
  refit_mlm_y <<- train_data[, 1]
  refit_mlm_test <<- test_data
  prediction_mlm <- vector("list", length = length(models$models))
  modelnames <- row.names(models$pred_accuracy)
  names(prediction_mlm) <- modelnames
  if (classification) {
    file <- "R/scratch/refit_class.py"
  } else {
    file <- "R/scratch/refit.py"
  }
  for (i in seq_len(length(models$models))) {
    refit_mlm_temp <<- models$models[[i]]
    reticulate::source_python(file)
    prediction_mlm[[i]] <- predictions
  }
  prediction_mlm
}
# Just return predictions
