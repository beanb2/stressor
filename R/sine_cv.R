sine_cv <- function(formula, data, n_folds = 10, k_mult = NULL) {
  x_data <- model.matrix(formula, data = data)[, -1]
  # Need to get the features, but really need the formula to do so (possibly
  #  do we pass the formula through to make it simple)
  if (!is.null(k_mult)){
    features <- scale(x_data)
    xvs <- cluster_part2(features, n_folds, k_mult)
  } else {
    xvs <- rep(1:n_folds,length = nrow(data))
    xvs <- sample(xvs)
  }
  predictions <- vector("numeric", length = nrow(data))
  for(i in seq_len(n_folds)){
    test_index <- which(i == xvs)
    train <- data[-test_index, ]
    test <- data[test_index, ]
    predictions[i == xvs] <- predict(lm(formula, data = train), test)
  }
  predictions
}
