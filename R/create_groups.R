create_groups <- function(formula, data, n_folds = 10, k_mult = NULL){
  x_data <- model.matrix(formula, data = data)[, -1]
  if (!is.null(k_mult)){
    features <- scale(x_data)
    xvs <- cluster_part2(features, n_folds, k_mult)
  } else {
    xvs <- rep(1:n_folds,length = nrow(data))
    xvs <- sample(xvs)
  }
  xvs
}
