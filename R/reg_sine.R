#' Sinusoidal Regression
#'
#' A simple example of sinusoidal regression that is in the form of
#'  \eqn{y = asin(b(x - c))} and is the sum of of multiple of these sine
#'  functions with a common intercept term.
#' @param formula A formula object to describe the relationship.
#' @param data The response and predictor variables.
#' @param method The method that is passed to the optim function by default it
#'  is the BFGS method which uses a gradient.
#' @param init_guess The initial parameter guesses for the optim function, by
#'  default it is all ones.
#' @param ... Additional arguments passed to the optim function
#' @return A "reg_sine" object is returned which contains the results from the
#'  optim function that was returned.
#' @export
reg_sine <- function(formula, data, method = "BFGS",
                     init_guess = rep(1, ncol(data) * 3 - 2), ...){
  temp <- model.frame(formula = formula, data = data)
  Y <- temp[, 1]
  X <- as.matrix(temp[, -1])

  obj <- sine_optimize(init_guess, X, Y, method, ...)
  attr(obj, "formula") <- terms(formula, data = data)
  class(obj) <- "reg_sine"
  obj
}

sine_function <- function(estimated, X, Y) {
  Y_pred <- sine_yhat(estimated, X)
  error <- sum((Y_pred - Y)^2, na.rm=TRUE)
  error
}

sine_yhat <- function(estimated, X) {
  vec_2 <- X
  est_mat <- matrix(estimated[-length(estimated)], nrow = 3, ncol = ncol(X))
  for (i in seq_len(ncol(X))) {
    amp <- est_mat[1, i]
    per <- est_mat[2, i]
    shift <- est_mat[3, i]
    vec_2[, i] <- amp * sin(per * (X[, i] - shift))
  }
  Y_pred <- rowSums(vec_2) + estimated[length(estimated)]
  Y_pred
}

sine_gradient <- function(estimated, X, Y) {
  vec_2 <- vector("numeric", length = length(estimated))
  Y_pred <- sine_yhat(estimated, X)
  est_mat <- matrix(estimated[-length(estimated)], nrow = 3, ncol = ncol(X))
  for (i in seq_len(ncol(est_mat))) {
    amp <- est_mat[1, i]
    per <- est_mat[2, i]
    shft <- est_mat[3, i]
    vec_2[3 * i - 2] <- sum((-1) * sin(per * (X[, i] - shft)))
    vec_2[3 * i - 1] <- sum((-1) * amp * (X[, i] - shft) *
                              cos(per * (X[, i] - shft)))
    vec_2[3 * i] <- sum(amp * per * cos(per * (X[, i] - shft)))
  }
  vec_2[length(estimated)] <- -1 * nrow(X)
  vec_2 <- sum(2 * (Y - Y_pred)) * vec_2
  vec_2
}

sine_optimize <- function(init_guess, X, Y, method, ...) {
  opt <- optim(init_guess, fn = sine_function,
               gr = sine_gradient, method = method, X, Y, ...)
}
