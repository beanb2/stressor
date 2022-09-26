#' Predict for Sinusoidal Regression
#'
#' Predict values on an additive exponential model
#' @param object A "reg_sine" object that has a formula attribute and parameter
#'   estimates from \link[reg_sine]{reg_sine}.
#' @param newdata Dataframe that has the same variables as the formula from
#'   "reg_sine".
#' @param ... Extending the [stats::predict()] function default, in this case
#'   this is ignored.
#' @return A vector with the predicted values.
#' @export
predict.reg_sine <- function(object, newdata, ...) {
  par <- object$par
  formula <- delete.response(formula(object))
  X <- model.matrix(formula, data = newdata)[, -1]
  if (!is.element("matrix", class(X))) {
    X <- as.matrix(X, ncol = 1)
  }
  y_pred <- sine_yhat(par, X)
  y_pred
}
