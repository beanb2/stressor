#' Predict for Sinusoidal Regression
#'
#' Predict values on an additive exponential model
#' @param object A "reg_sine" object that has a formula attribute and parameter
#'  estimates from \link[reg_sine]{reg_sine}.
#' @param newdata Dataframe that has the same variables as the formula from
#'  "reg_sine".
#' @return A vector with the predicted values.
#' @export
predict.reg_sine <- function(object, newdata, ...) {
  par <- object$par
  X <- model.matrix(attr(object, "formula"), data = newdata)[, -1]
  y_pred <- sine_yhat(par, X)
  y_pred
}
