cv.reg_sine <- function(object, data, n_folds = 10, k_mult = NULL) {
  groups <- create_groups(formula(object), data, n_folds, k_mult)

}
