# Regression Function for Asymptotic
# Y = a - b * e^(-c * X_1) - d * e^(-f * X_2) - ... - g * e^(-h * X_n)
reg_asym <- function(formula, data, method = "BFGS",
                     init_guess = rep(1, ncol(data) * 2 - 1), ...) {
  temp <- model.frame(formula = formula, data = data)
  X <- temp[, -1]
  Y <- temp[, 1]
  obj <- asym_optimize(init_guess, X, Y, method = method, ...)
  class(obj) <- "reg_asym"
  obj
}

predict.reg_asym <- function(object, newdata, ...) {
  print("TODO:Implement after talking to Dr. Bean")
}

asym_function <- function(estimated, X, Y) {
  Y_pred <- asym_yhat(estimated, X)
  error <- sum((Y_pred - Y)^2, na.rm=TRUE)
  error
}

asym_gradient <- function(estimated, X, Y) {
  Y_pred <- asym_yhat(estimated, X)
  vec_2 <- vector(length = length(estimated))
  est_mat <- matrix(estimated[-length(estimated)], nrow = 2, ncol = ncol(X))
  for (i in seq_len(ncol(X))) {
    prin <- est_mat[1, i]
    rate <- est_mat[2, i]
    vec_2[2 * i - 1] <- sum(exp(-rate * X[, i]))
    vec_2[2 * i] <- sum(-prin * X[, i] * exp(-rate * X[, i]))
  }
  vec_2[length(estimated)] <- -1 * nrow(X)
  vec_2 <- sum(2 * (Y - Y_pred)) * vec_2
  vec_2
}

asym_yhat <- function(estimated, X) {
  vec_2 <- X
  est_mat <- matrix(estimated[-length(estimated)], nrow = 2, ncol = ncol(X))
  for (i in seq_len(ncol(X))) {
    prin <- est_mat[1, i]
    rate <- est_mat[2, i]
    vec_2[, i] <- -prin * exp(-rate * X[, i])
  }
  Y_pred <- estimated[length(estimated)] + rowSums(vec_2)
  Y_pred
}

asym_optimize <- function(init_guess, X, Y, method, ...) {
  opt <- optim(init_guess, fn = asym_function, gr = asym_gradient,
               method = method, X, Y, ...)
}

data_gen_asym <- function(n, weight_mat = matrix(rlnorm(10), nrow = 2, ncol = 5),
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
    prin <- weight_mat[1, i]
    rate <- weight_mat[2, i]
    vec_2[, i] <- -prin * exp(-rate * vec_1[, i])
  }
  Y <- y_int + rowSums(vec_2) + eps
  vec_1 <- cbind(Y, vec_1)
  as.data.frame(vec_1)
}

# Unit tests
# data_gen
# Predict
# Set up simulation study, accuracy of the method as epsilon increases with
#  plotting the expected, theoretical, and epsilon size. Sample size. Small sets.
# PyCaret
