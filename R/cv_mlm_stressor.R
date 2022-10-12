#' Cross Validation for mlm_stressor
#'
#' This is the cross validation wrapper for the mlm_stressor method
#' @param object A reg_sine object.
#' @param data A data-frame object to be used for cross-validation
#' @param n_folds An integer value for the number of folds defaulted to 10. If
#'   NULL, it will run LOO cross validation.
#' @param k_mult Used to specify if k-means clustering is to be used, defaulted
#'   to NULL.
#' @return A data frame of cross-validated predictions where the columns are the
#'   various methods of machine learning models.
#' @importFrom stats formula
#' @examples
#'  lm_test <- data_gen_lm(10)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test, example = TRUE)
#'  cv(mlm_lm, lm_test, n_folds = 5)
#' @export
cv.mlm_stressor <- function(object, data, n_folds = 10, k_mult = NULL) {
  groups <- create_groups(formula(object), data, n_folds, k_mult)
  predictions <- cv_core(object, data, groups)
  predictions
}



