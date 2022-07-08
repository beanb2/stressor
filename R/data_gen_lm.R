#' Data Generation for Linear Regression
#'
#' Creates a synthetic dataset from various parameters that represent a simple
#'  example of an additive \eqn{y = mx}.
#' @param n The number of observations for each parameter.
#' @param weight_vec The parameter coefficients where each entry represents the
#'  coefficients for the additive linear model.
#' @param y_int The y-intercept term of the additive model.
#' @param resp_sd The standard deviation of the epsilon term to be added for
#'  noise.
#' @return A dataframe object with the n rows and the response variable with
#'  the number of parameters being equal to the number of columns from the
#'  weight matrix.
#' @export
data_gen_lm <- function(n, weight_vec = rep(1, 5), y_int = 0,
                        resp_sd = 1, ...) {
  # This chunk makes the predictor variables
  vec_1 <- rnorm(n * length(weight_vec))
  vec_1 <- matrix(vec_1, nrow = n, ncol = length(weight_vec))

  colnames(vec_1) <- paste0(rep("V", length(weight_vec)),
                            seq(length(weight_vec)))
  # Create the Response Variable with the specified weights
  eps <- rnorm(n, sd = resp_sd)
  Y <- as.vector(t(weight_vec) %*% t(vec_1)) + y_int + eps
  vec_1 <- cbind(Y, vec_1)
  as.data.frame(vec_1)
}
