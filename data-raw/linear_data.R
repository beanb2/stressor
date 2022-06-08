# Code to make LINEAR DATASET
data_gen_lm <- function(n, weight_vec, resp_sd, ...){
  # This chunk makes the predictor variables
  vec_1 <- rnorm(n * length(weight_vec))
  vec_1 <- matrix(vec_1, nrow = n, ncol = length(weight_vec))

  colnames(vec_1) <- paste0(rep("V", length(weight_vec)),
                                seq(length(weight_vec)))
  # Create the Response Variable with the specified weights
  eps <- rnorm(n, sd = resp_sd)
  Y <- as.vector(t(weight_vec) %*% t(vec_1)) + eps
  vec_1 <- cbind(Y, vec_1)
  as.data.frame(vec_1)
}
