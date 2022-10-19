#' Predict for Machine Learning Models Object
#'
#' A prediction method to fit predictions given test data as well as can refit
#'  and predict if given training data to refit on.
#' @param object A `"mlm_stressor"` object
#' @param newdata A data.frame object that is to be predicted
#' @param train_data Defaulted to `NULL`, but a data.frame when specified for
#'  the new training to take place.
#' @param ... Additional arguments that have to be overwritten currently not be
#'  passed on.
#' @return A data.frame object where the columns are the various methods and the
#'  rows are the predictions
#' @examples
#'  lm_test <- data_gen_lm(10)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test, example = TRUE)
#'  predict(mlm_lm, lm_test)
#' @export
predict.mlm_stressor <- function(object, newdata, train_data = NULL, ...) {
  data_check(formula(object), newdata, train_data)
  classification = FALSE
  if (class(object)[2] == "classifier") {classification = TRUE}
  pred <- mlm_refit(object, train_data, test_data = newdata, classification)
  pred <- as.data.frame(pred)
  pred
}
