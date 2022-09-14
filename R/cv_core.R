cv_core <- function(object, data, t_groups) {
  curr_methods <- c("reg_sine", "reg_asym", "lm", "mlm_stressor")
  method <- class(object)
  if (!is.element(method, curr_methods)){
    stop("Not a supported method at this time!")
  }
  if (method == "mlm_stressor") {
    predictions <- matrix(0, nrow = nrow(data),
                          ncol = length(object$models))
  } else {
    predictions <- vector("numeric", length = nrow(data))
  }
  for (i in seq_len(max(t_groups))) {
    test_index <- which(i == t_groups)
    train <- data[-test_index, ]
    test <- data[test_index, ]
    if (method == "mlm_stressor") {
      predictions[test_index, ] <- refit_mlm(object, train, test)
    } else if (method == "reg_sine") {
      predictions[test_index] <- predict(reg_sine(formula(object),
                                                       data = train), test)
    } else if (method == "reg_asym") {
      predictions[test_index] <- predict(reg_asym(formula(object),
                                                       data = train), test)
    } else if (method == "lm"){
      predictions[test_index] <- predict(lm(formula(object),
                                                 data = train), test)
    } else {
      stop("Current method is unsupported at this time.")
    }
  }
  if (method == "mlm_stressor") {
    colnames(predictions) <- row.names(object$pred_accuracy)
    predictions <- as.data.frame(predictions)
  }
  predictions
}
