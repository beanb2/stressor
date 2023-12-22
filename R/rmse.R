#' Root Mean Square Error
#'
#' Calculates the root mean square error.
#' @param predictions A data frame or vector object that is the same number of
#'   rows or length as the length of observed values.
#' @param observed A vector of the observed results
#' @return  A finite value or a data frame of methods and their RMSE values
#' @examples
#'   lm_data <- data_gen_lm(25)
#'   test_index <- sample(1:nrow(lm_data), 5)
#'   test <- lm_data[test_index, ]
#'   train <- lm_data[-test_index, ]
#'   lm_test <- lm(Y ~ ., train)
#'   lm_cv <- cv(lm_test, lm_data, n_folds = 5)
#'   lm_cv
#' @export
rmse <- function(predictions, observed) {
  if (is.null(ncol(predictions))) {
    rmse <- sqrt(sum((observed - predictions)^2) / length(predictions))
  } else {
    rmse <- vector("numeric", length = ncol(predictions))
    for (i in seq_len(ncol(predictions))) {
      rmse[i] <- sqrt(mean((observed - predictions[, i])^2))
    }
    models <- colnames(predictions)
    rmse <- data.frame(models, rmse)
  }
  rmse
}
