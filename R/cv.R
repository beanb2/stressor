#' Cross Validation
#'
#' This is core of cross validation both standard and using k-mean groups. This
#'  method is called by other cv methods of classes.
#' @param object One of the four objects that is accepted mlm_stressor,
#'   reg_sine, reg_asym, or lm.
#' @param data A data frame object that contains all the entries to be cross-
#'   validated on.
#' @param n_folds An integer value for the number of folds defaulted to 10. If
#'   NULL, it will run LOO cross validation.
#' @param k_mult Used to specify if k-means clustering is to be used, defaulted
#'   to NULL.
#' @return If the object is of class mlm_stressor then a data frame will be
#'   returned otherwise a vector of the predictions will be returned.
#' @examples
#'  lm_test <- data_gen_lm(10)
#'  lm <- lm(Y ~ ., lm_test)
#'  lm_cv <- cv(lm, lm_test)
#'  lm_cv
#' @export
cv <- function(object, data, n_folds = 10, k_mult = NULL) {
  UseMethod("cv")
}

