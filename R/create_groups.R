#' Create groups for CV
#'
#' Creates groups for the data by separating them either into 10 fold cross-
#'   validation, LOO cross-validation, or k-means grouping.
#' @param formula A formula object that fits the data
#' @param data The data that will be separated into each group
#' @param n_folds An integer value defaulted to 10 fold cross-validation if NULL
#'   uses LOO instead.
#' @param k_mult When specified is passed onto the [stressor::cv_cluster()]
#'   to fit the data into k_groups
#' @return A vector of the assigned groups
#' @examples
#'  # Data generation
#'  lm_data <- data_gen_lm(20)
#'
#'  # 10 Fold CV group
#'  create_groups(Y ~ ., lm_data)
#'  create_groups(Y ~ ., lm_data, n_folds = 10, k_mult = NULL)
#'
#'  # LOO CV group
#'  create_groups(Y ~ ., lm_data, n_folds = NULL)
#'
#'  # Clustered CV group
#'  create_groups(Y ~ ., lm_data, n_folds = 10, k_mult = 5)
#' @importFrom stats model.matrix
#' @export
create_groups <- function(formula, data, n_folds = 10, k_mult = NULL){
  x_data <- model.matrix(formula, data = data)[, -1]
  if (!is.null(n_folds)){
    xvs <- rep(1:n_folds,length = nrow(data))
    xvs <- sample(xvs)
  } else if (!is.null(k_mult)){
    features <- scale(x_data)
    xvs <- cv_cluster(features, n_folds, k_mult)
  } else {
    xvs <- seq_len(nrow(data))
  }
  xvs
}
