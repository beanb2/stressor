#' @describeIn cv Cross Validation for mlm_stressor
#' @importFrom stats formula
#' @examples
#'  # Machine Learning Models example
#'  lm_test <- data_gen_lm(10)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test, example = TRUE)
#'  cv(mlm_lm, lm_test, n_folds = 5)
#'
#' @export
cv.mlm_stressor <- function(object, data, n_folds = 10, k_mult = NULL,
                            repl = FALSE) {
  data_check(formula(object), data)
  integer_check(n_folds)
  groups <- create_groups(formula(object), data, n_folds, k_mult, repl)
  predictions <- cv_core(object, data, groups)
  predictions
}



