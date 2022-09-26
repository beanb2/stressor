#' Refit Machine Learning Models
#'
#' Refits models fitted in the [stressor::mlm_init()], and returns the
#'  predictions.
#' @param mlm_object The returned object from either [stressor::mlm_regressor()]
#'  or [stressor::mlm_classification].
#' @param train_data A dataframe object used for refitting excludes the test
#'   data, making sure that the response variable is first.
#' @param test_data A dataframe object used for predictions, making sure that
#'   the response variable is first.
#' @param classification A boolean value used to represent if classification
#'   methods need to be used to refit the data.
#' @return A matrix with the predictions of the various machine learning
#'   methods.
#' @export
mlm_refit <- function(mlm_object, train_data, test_data,
                      classification = FALSE) {
  if (classification) {
    file <- system.file("python", "refit_class.py", package = "stressor")
  } else {
    file <- system.file("python", "refit.py", package = "stressor")
  }
  refit_mlm_X <<- train_data[, -1]
  refit_mlm_y <<- train_data[, 1]
  refit_mlm_test <<- test_data
  prediction_mlm <- matrix(0, nrow = nrow(test_data),
                           ncol = length(mlm_object$models))
  modelnames <- row.names(mlm_object$pred_accuracy)
  colnames(prediction_mlm) <- modelnames
  for (i in seq_len(length(mlm_object$models))) {
    refit_mlm_temp <<- mlm_object$models[[i]]
    reticulate::source_python(file)
    prediction_mlm[, i] <- predictions[, "Label"]
  }
  prediction_mlm
}

