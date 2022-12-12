#' Create groups for CV
#'
#' Creates groups for the data by separating them either into 10 fold cross-
#'   validation, LOO cross-validation, or k-means grouping.
#' @param formula A formula object that specifies the
#' @param data The data that will be separated into each group
#' @param n_folds An integer value defaulted to 10 fold cross-validation if NULL
#'   uses Leave One Out(LOO) instead.
#' @param k_mult When specified is passed onto the \link[stressor]{cv_cluster}
#'   to fit the data into k_groups
#' @param repl A Boolean value defaulted to `FALSE`, change to `TRUE` when
#'   replicates need to be included in the same group.
#' @return A vector of the length equal to number of rows of data.frame from the
#'   data argument.
#' @details If `k_mult` is specified as an integer the formula object will be
#'   used to help determine the features specified by the user. Which will be
#'   passed to the \link[stressor]{cv_cluster} function. Which takes a scaled
#'   matrix of features.
#'
#'   This function is called by the \link[stressor]{cv} methods as it forms the
#'    groups necessary to perform the cross validation. If you were wanting to
#'    use for your own use. It is nice function that separates the `data` into
#'    groups for training and testing.
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
#' @importFrom dplyr distinct left_join
#' @export
create_groups <- function(formula, data, n_folds = 10, k_mult = NULL,
                          repl = FALSE, group_by = NULL){
  data_check(formula, data)
  integer_check(n_folds)
  boolean_check(repl)
  x_data <- model.matrix(formula, data = data)[, -1]
  if (repl) {
    x_data_copy <- as.data.frame(x_data)
    x_data <- as.data.frame(x_data)
    x_data <- dplyr::distinct(x_data)
  }
  if (!is.null(n_folds)){
    xvs <- rep(1:n_folds,length = nrow(x_data))
    xvs <- sample(xvs)
  } else if (!is.null(k_mult)){
    integer_check(k_mult)
    features <- scale(as.matrix(x_data))
    xvs <- cv_cluster(features, n_folds, k_mult)
  } else {
    xvs <- seq_len(nrow(x_data))
  }
  if (repl) {
    x_data$xvs <- xvs
    x_data <- dplyr::left_join(x_data_copy, x_data)
    xvs <- x_data$xvs
  }
  xvs
}
