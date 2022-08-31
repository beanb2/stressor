mlm_thinning <- function(mlm_object, data, max = .95, min = .05, iter = .05) {
  train_size <- seq(max, min, iter)
  predictions <- vector("list", length = length(train_size))
  for (i in seq_len(length(train_size))) {
    train_index <- sample(1:nrow(data), train_size[i] * nrow(data))
    test <- data[-train_index, ]
    train <- data[train_index, ]
    predictions[[i]] <- refit_mlm(mlm_original, train, test)
  }
}
