mlm_kmeans <- function(mlm_original, data, features, k_groups = 10){
  groups <- cluster_part2(features, k = k_groups)
  predictions <- vector("list", length = k_groups)
  for (i in seq_len(k_groups)){
    test_index <- which(i == k_groups)
    test <- data[test_index, ]
    train <- data[-test_index, ]
    predictions[[i]] <- refit_mlm(mlm_original, train, test)
  }
  names(predictions) <- seq(1, k_groups, 1)
  predictions
}
