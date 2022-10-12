#' Distance to Center
#'
#' Calculates the distance from center of the X parameter space.
#' @param formula A formula object
#' @param data A data frame object
#' @return A vector of distances from the center.
#' @examples
#'   data <- data_gen_lm(10)
#'   dist <- dist_cent(Y ~ ., data)
#'   dist
#' @importFrom stats model.matrix model.response model.frame
#' @export
dist_cent <- function(formula, data){
  t_data <- model.matrix(formula, data = data)[, -1]
  t_response <- model.response(model.frame(formula, data = data))
  scaled_data <- scale(t_data)
  center <- apply(scaled_data, 2, mean)
  dist <- vector("numeric", length = nrow(data))
  for (i in seq_len(length(dist))){
    dist[i] <- euclid_dist(center, data[i, ])
  }
  dist
}

euclid_dist <- function(x, y) {
  sqrt(sum((x - y)^2))
}
