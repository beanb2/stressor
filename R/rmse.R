#' Root Mean Square Error
#'
#' Calculates the root mean square error.
#' @param predictions A data frame or vector object that is the same number of
#'   rows or length as the length of observed values.
#' @param observed A vector of the observed results
#' @return  A finite value or a data frame of methods and their rmse values
#' @examples
#'   # Using vignette data
#'   data(boston)
#'   data(mlm_vignette_boston_cv)
#'   rmse_boston <- rmse(mlm_vignette_boston_cv, boston$cmedv)
#'   rmse_boston
#' @export
rmse <- function(predictions, observed) {
  if (is.null(ncol(predictions))) {
    rmse <- sqrt(sum((observed - predictions[, i])^2) / nrow(predictions))
  } else {
    rmse <- vector("numeric", length = ncol(predictions))
    for (i in seq_len(ncol(predictions))) {
      rmse[i] <- sqrt(sum((observed - predictions[, i])^2) / nrow(predictions))
    }
    models <- colnames(predictions)
    rmse <- data.frame(models, rmse)
  }
  rmse
}
