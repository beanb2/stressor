mlm_cv <- function(formula, data, mlm_object, n_folds = 10, k_mult = NULL) {
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
  predictions <- vector(mode = "list", length = n_folds)
  for(i in seq_len(n_folds)){
    test_index <- which(i == xvs)
    train <- data[-test_index, ]
    test <- data[test_index, ]
    predictions[[i]] <- refit_mlm(mlm_object, train, test)
  }
  if (!is.null(k_mult)) {
    names(predictions) <- paste0(seq(1, n_folds, 1), rep('group', n_folds))
  } else {
    names(predictions) <- paste0(seq(1, n_folds, 1), rep('fold', n_folds))
  }
  predictions
  # Possible problem is that we don't have exactly what these predictions are
  #  being compared to
}


# errors <- cross_validation_error(predictions, attr(mlm_object, "response"))
# rmse <- matrix(0, nrow = length(mlm_object$models), ncol = 1)
# for (i in seq_len(ncol(errors))) {
#   errors[, i] <- (nrow(predictions[[i]][[1]]) / nrow(data)) * errors[, i]
# }
# rmse[, 1] <- rowSums(errors)
# row.names(rmse) <- row.names(mlm_object$pred_accuracy)
# colnames(rmse) <- "rmse"
# obj <- list(rmse = rmse, predictions = predictions)
# obj
