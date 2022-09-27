#' Create groups for CV
#'
#' Creates groups for the data by separating them either into 10 fold cross-
#'   validation, LOO cross-validation, or k-means grouping.
#' @param formula A formula object that fits the data
#' @param data The data that will be separated into each group
#' @param n_folds An integer value defaulted to 10 fold cross-validation if NULL
#'   uses LOO instead.
#' @param k_mult When specified is passed onto the \code{\link{cluster_part2}}
#'   to fit the data into k_groups
#' @return A vector of the assigned groups
#' @export
create_groups <- function(formula, data, n_folds = 10, k_mult = NULL){
  x_data <- model.matrix(formula, data = data)[, -1]
  if (!is.null(n_folds)){
    xvs <- rep(1:n_folds,length = nrow(data))
    xvs <- sample(xvs)
  } else if (!is.null(k_mult)){
    features <- scale(x_data)
    xvs <- cluster_part2(features, n_folds, k_mult)
  } else {
    xvs <- seq_len(nrow(data))
  }
  xvs
}
