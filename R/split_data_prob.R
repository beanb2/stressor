#' Create Train Index Set
#'
#' This function takes in a data.frame object and the training size and returns
#'   a logical vector indicating which entries to include.
#' @param data A data.frame object used to determine the length of the vector.
#' @param train_size A numeric that is between zero and one.
#' @return A logical vector is returned that is the same length as the number of
#'   rows of the data.
#' @examples
#'   lm_data <- data_gen_lm(10)
#'   indices <- split_data_prob(lm_data, .8)
#'   train <- lm_data[indices, ]
#'   test <- lm_data[!indices, ]
#' @export
split_data_prob <- function(data, train_size) {
  numeric_check(train_size)
  if (!(train_size >= 0 && train_size <= 1)) {
    stop("Training size must be between 0 and 1.")
  }
  sample(c(TRUE, FALSE), nrow(data), replace = TRUE,
         prob = c(train_size, 1 - train_size))
}
