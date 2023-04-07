#' Thinning algorithm for Models with Predict function
#'
#' Fits various train size and test sizes.
#' @param model a model that is currently of class type "reg_sine", "reg_asym",
#'   "lm", or "mlm_stressor".
#' @param data a data frame with all the data
#' @param max a numeric value in (0, 1] and greater than `min`,
#'  defaulted to .95.
#' @param min a numeric value in (0, 1) and less than `max`, defaulted to .05.
#' @param iter a numeric value to indicate the step size, defaulted to .05
#' @return a list object where the first element is the RMSE values at each
#'   iteration and the second element being the predictions.
#' @examples
#'  lm_test <- data_gen_lm(20)
#'  create_virtualenv()
#'  mlm_lm <- mlm_regressor(Y ~ ., lm_test, example = TRUE)
#'  thin <- thinning(mlm_lm, lm_test, max = .8, min = .7, iter = .05)
#'  thin
#' @export
thinning <- function(model, data, max = .95, min = .05, iter = .05) {
  train_size <- seq(min, max, iter)
  curr_methods <- c("reg_sine", "reg_asym", "lm", "mlm_stressor")
  method <- class(model)[1]
  if (!is.element(method, curr_methods)){
    stop("Not a supported method at this time!")
  }
  predictions <- vector("list", length = length(train_size))
  pred_rmse <- vector("numeric", length = length(train_size))
  for (i in seq_len(length(train_size))) {
    train_index <- sample(1:nrow(data), train_size[i] * nrow(data))
    test <- data[-train_index, ]
    train <- data[train_index, ]
    if (method == "mlm_stressor") {
      classification = FALSE
      if (class(model)[2] == "classifier") {classification = TRUE}
      predictions[test_index, ] <- mlm_refit(model, train, test,
                                             classification)
    } else if (method == "reg_sine") {
      predictions[[i]] <- predict(reg_sine(formula(model),
                                                  data = train), test)
    } else if (method == "reg_asym") {
      predictions[[i]] <- predict(reg_asym(formula(model),
                                                  data = train), test)
    } else if (method == "lm"){
      predictions[[i]] <- predict(lm(formula(model),
                                            data = train), test)
    } else {
      stop("Current method is unsupported at this time.")
    }
    pred_rmse[i] <- rmse(predictions[[i]], test$Y)
  }
  rtn_list <- list(RMSE = pred_rmse, predictions = predictions)
  rtn_list
}
