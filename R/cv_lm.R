#' Cross Validation for lm
#'
#' This is the cross validation wrapper for the lm method.
#' @param object A lm object.
#' @param data A data-frame object to be used for cross-validation
#' @param n_folds An integer value for the number of folds defaulted to 10. If
#'   NULL, it will run LOO cross validation.
#' @param k_mult Used to specify if k-means clustering is to be used, defaulted
#'   to NULL.
#' @return A vector of cross-validated predictions.
#' @examples
#'  lm_test <- data_gen_lm(10)
#'  lm <- lm(Y ~ ., lm_test)
#'  cv(lm, lm_test, n_folds = 5)
#' @importFrom stats formula
#' @export
cv.lm <- function(object, data, n_folds = 10, k_mult = NULL) {
  groups <- create_groups(formula(object), data, n_folds, k_mult)
  predictions <- cv_core(object, data, groups)
  predictions
}
