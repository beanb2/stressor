# Code to make LINEAR DATASET
linear_data <- function(n, weight_vec, ...){
  # This chunk makes the predictor variables
  vec_1 <- rnorm(n)
  data_test <- cbind(vec_1)
  for (i in seq_len(length(weight_vec) - 1)) {
    vec_2 <- rnorm(n)
    data_test <- cbind(data_test, vec_2)
  }
  colnames(data_test) <- NULL

  colnames(data_test) <- paste0(rep("V", length(weight_vec)),
                                seq(length(weight_vec)))
  # Create the Response Variable with the specified weights
  eps <- rnorm(n)
  Y <- as.vector(t(weight_vec) %*% t(data_test)) + eps
  data_test <- cbind(Y, data_test)
  data_test
}
