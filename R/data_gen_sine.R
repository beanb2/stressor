#' Data Generation for Sinusoidal Regression
#'
#' Creates a synthetic data set from various parameters that represent a simple
#'  example of an additive \eqn{y = asin(b(x - c))}.
#' @param n The number of observations for each parameter.
#' @param weight_mat The parameter coefficients where each column represents
#'  the coefficients and is three rows as each additive equation contains three
#'  parameters. Defaulted to be 15 random numbers from the normal distribution.
#' @param y_int The y-intercept term of the additive model.
#' @param resp_sd The standard deviation of the epsilon term to be added for
#'  noise.
#' @param ... additional arguments that are not currently implemented
#' @return A data frame object with the n rows and the response variable with
#'  the number of parameters being equal to the number of columns from the
#'  weight matrix.
#' @examples
#'  # Generates 10 observations
#'  sine_data <- data_gen_sine(10)
#'  sine_data
#' @importFrom stats rnorm
#' @export
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

# Create if else to catch the case for testing
