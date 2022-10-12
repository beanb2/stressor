#' Residual Value Calculation
#'
#' Helper function when wanting to calculate the residual values for the
#'  machine learning models as they return a data frame of predictions. This
#'  function also handles vectors as well.
#' @param predictions A vector or data frame of predicted values
#' @param observed A vector of the observed values
#' @return A data frame or a vector depending on the input of the predictions
#'  parameter
#' @examples
#'  pred <- matrix(rnorm(50), ncol = 5, nrow = 10)
#'  obs <- rnorm(10)
#'  res <- residual(pred, obs)
#' @export
residual <- function(predictions, observed){
  if (is.null(ncol(predictions))) {
    resid <- observed - predictions
  } else {
    resid <- matrix(0, nrow = nrow(predictions), ncol = ncol(predictions))
    for (i in seq_len(ncol(predictions))) {
      resid[, i] <- observed - predictions[, i]
    }
    colnames(resid) <- colnames(predictions)
    resid <- as.data.frame(resid)
  }
  resid
}
