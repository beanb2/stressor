mlm_cv <- function(mlm_object, data, n_folds = 10) {
  xvs <- rep(1:n_folds,length = nrow(data))
  # Add the switch case for k-means cross validation
  xvs <- sample(xvs)
  predictions <- vector(mode = "list", length = n_folds)
  for(i in seq_len(n_folds)){
    train <- data[xvs!=i, ]
    test <- data[xvs==i, ]
    predictions[[i]] <- refit_mlm(mlm_object, train, test)
  }
  names(predictions) <- paste0(seq(1, n_folds, 1), rep('fold', n_folds))
  predictions
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
