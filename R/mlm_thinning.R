#' Thinning algorithm for Machine Learning Models
#'
#' Fits various train size and test sizes.
#' @param mlm_object a `"mlm_stressor"` object that has models in the first list
#'  object.
#' @param data a data frame with all the data
#' @param max a numeric value in (0, 1] and greater than `min`,
#'  defaulted to .95.
#' @param min a numeric value in (0, 1) and less than `max`, defaulted to .05.
#' @param iter a numeric value to indicate the step size, defaulted to .05
#' @return a list object where each item in the list is one step in the thinning
#'  process and is a data frame of predictions.
#' @examples
#'  lm_test <- data_gen_lm(10)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test, example = TRUE)
#'  thin <- mlm_thinning(mlm_lm, lm_test, max = .8, min = .7, iter = .05)
#'  thin
#' @export
mlm_thinning <- function(mlm_object, data, max = .95, min = .05, iter = .05) {
  train_size <- seq(min, max, iter)
  predictions <- vector("list", length = length(train_size))
  for (i in seq_len(length(train_size))) {
    train_index <- sample(1:nrow(data), train_size[i] * nrow(data))
    test <- data[-train_index, ]
    train <- data[train_index, ]
    predictions[[i]] <- mlm_refit(mlm_object, train, test)
  }
}
