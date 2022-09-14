#' Cross Validation
#'
#' This is core of cross validation both standard and using k-mean groups. This
#'  method is called by other cv methods of classes.
#' @param object One of the four objects that is accepted mlm_stressor,
#'   reg_sine, reg_asym, or lm.
#' @param data A data frame object that contains all the entries to be cross-
#'   validated on.
#' @param t_groups A vector of how the data entries are assigned to each group.
#' @return If the object is of class mlm_stressor then a data frame will be
#'   returned otherwise a vector of the predictions will be returned.
cv <- function(object, data, n_folds = 10, k_mult = NULL) {
  UseMethod("cv")
}
