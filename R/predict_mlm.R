compare_mlm <- function(formula, data, n_models = 9999, ...) {
  temp <- model.frame(formula = formula, data = data)
  vv <- attr(terms(formula(temp)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name

  message("Importing Pycaret Regression")
  reg <- reticulate::import("pycaret.regression")
  data_test <- sample(nrow(data), .1 * nrow(data))
  test <- test_sine[data_test, ]
  data <- test_sine[-data_test, ]
  # Need to take it off the parallel process to run -
  #  Github reference from Pycaret
  message("Setting up the data for fitting a models. Please press return key.")
  exp_reg <- reg$setup(data = data, target = rr, n_jobs = as.integer(1), ...)

  # Does not tune the models, finely, we may want to add a function that
  #  lets the user specify the creation

  # TODO: Create a PyScript that returns the predictive Accuracy
  # Change the number to 9999 as a "bogus number"
  message("Fitting Machine Learning Models")
  models <- reg$compare_models(n_select = as.integer(n_models))
  Model <- reg$pull()$Model
  rmse <- vector(mode = "numeric", length = length(Model))

  pred_accuracy <- data.frame(Model, rmse)
  for (i in seq_len(length(models))) {
    pred_results <- reg$predict_model(models[[i]], data = test)
    pred_accuracy[i, 2] <- sqrt(sum((test$Y - pred_results$Label)^2) /
                                  nrow(test))
  }
  pred_accuracy
}

# Can Save models for future use, by saving the pipeline
#  Are we wanting the prediction part to be separate?
#  Do we want to have a function that tunes a model?

# Can we tune the models and then use the clustered cross validation function
#  Call python to tune the model once we give the data
#  Looking more specific at scykit to call the individual mlm. Pushing the
#   parameters from pycaret to retrain.

# Want return predictions

# Already built-in fit function!

# Saving models as a group or individual
