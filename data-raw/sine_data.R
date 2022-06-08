# Code to make sine wave regression
# 1. Sample from normal/uniform/log-normal or whatever I want
# 2. Fit each sample to a sin curve using optim to approximate those values
# 3. Do this n times
# 5. Then to generate the response use all the saved sine function and sum them
#     together plus an intercept and epsilon term.

# Predict function
SSE <- function(estimated, actual) {
  sum((estimated - actual)^2,
      na.rm=TRUE)
}

datam <- rlnorm(100)
sine_function <- function(x, datam) {
  sine <- x[1] * sin(x[2] * (datam - x[3])) + x[4]
  error <- SSE(sine, datam)
}

# Optimize function
opt <- optim(c(1,1,1,1), fn = sine_function, datam = datam)

# Set a default matrix of coefficients
data_gen_sine <- function(n, weight_mat = matrix(rnorm(15), nrow = 3, ncol =5),
                          resp_sd = 1, ...) {
  # Produce the Predictor variables
  vec_1 <- rnorm(n * ncol(weight_mat))
  vec_1 <- matrix(vec_1, nrow = n, ncol = ncol(weight_mat))

  colnames(vec_1) <- paste0(rep("V", ncol(weight_mat)),
                            seq(ncol(weight_mat)))
  # Produce the response variable with noise
  eps <- rnorm(n, sd = resp_sd)
  y_int <- rnorm(1)
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
