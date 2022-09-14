#' Cross Validation for reg_sine
#'
#' This is the cross validation wrapper for the reg_sine method.
#' @param object A reg_sine object.
#' @param data A data-frame object to be used for cross-validation
#' @param n_folds An integer value for the number of folds defaulted to 10.
#' @param k_mult Used to specify if k-means clustering is to be used, defaulted
#'   to NULL.
#' @return A vector of cross-validated predictions.
#' @export
cv.reg_sine <- function(object, data, n_folds = 10, k_mult = NULL) {
  groups <- create_groups(formula(object), data, n_folds, k_mult)
  predictions <- cv_core(object, data, groups)
  predictions
}
