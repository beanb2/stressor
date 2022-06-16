# Model function
reg_sine <- function(formula, data, method = "BFGS", ...){
  temp <- model.frame(formula = formula, data = data)
  Y <- temp[, 1]
  X <- as.matrix(temp[, -1])

  # Find the formula using the optim function
  s <- optimize_sine(X, Y, method)
  s

  # class(s) <- "reg_sine"

}

sine_function <- function(estimated, X, Y) {
  Y_pred <- sine_yhat(estimated, X, Y)
  error <- SSE(Y_pred, Y)
}

sine_yhat <- function(estimated, X, Y) {
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
  Y_pred <- sine_yhat(estimated, X, Y)
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
  # Ask Dr. Bean if it should be the sum of -1
  vec_2[length(estimated)] <- -1 * nrow(X)
  vec_2 <- sum(2 * (Y - Y_pred)) * vec_2
  vec_2
}

optimize_sine <- function(X, Y, method) {
  opt <- optim(rep(1, ncol(X) * 3 + 1), fn = sine_function,
               gr = sine_gradient, method = method, X, Y)
}


# Predict function
SSE <- function(estimated, actual) {
  sum((estimated - actual)^2,
      na.rm=TRUE)
}

# Set a default matrix of coefficients
data_gen_sine <- function(n, weight_mat = matrix(rnorm(15), nrow = 3, ncol =5),
                          y_int = 0, resp_sd = 1, ...) {
  # Produce the Predictor variables
  vec_1 <- rnorm(n * ncol(weight_mat))
  vec_1 <- matrix(vec_1, nrow = n, ncol = ncol(weight_mat))

  colnames(vec_1) <- paste0(rep("V", ncol(weight_mat)),
                            seq(ncol(weight_mat)))
  # Produce the response variable with noise
  eps <- rnorm(n, sd = resp_sd)
  vec_2 <- vec_1
  for (i in seq_len(ncol(weight_mat))) {
    amp <- weight_mat[1, i]
    per <- weight_mat[2, i]
    shift <- weight_mat[3, i]
    vec_2[, i] <- amp * sin(per * (vec_1[, i] - shift))
  }
  Y <- rowSums(vec_2) + y_int + eps
  vec_1 <- cbind(Y, vec_1)
  as.data.frame(vec_1)
}

# Pycarat
#
