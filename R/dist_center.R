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
