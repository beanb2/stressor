# TODO: Add the models parameter with defaulted all the models
compare_mlm <- function(formula, data, n_models = 9999, ...) {
  temp <- model.frame(formula = formula, data = data)
  vv <- attr(terms(formula(temp)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name

  message("Importing Pycaret Regression")
  # TODO: Add in Classification
  reg <- reticulate::import("pycaret.regression")
  data_test <- sample(nrow(data), .1 * nrow(data))
  test <- test_sine[data_test, ]
  data <- test_sine[-data_test, ]
  # Need to take it off the parallel process to run -
  #  Github reference from Pycaret
  message("Setting up the data for fitting a models. Please press return key.")
  # exp_reg <- reg$setup(data = data, target = rr, n_jobs = as.integer(1), ...)
  #
  # # Does not tune the models, finely, we may want to add a function that
  # #  lets the user specify the creation
  # # TODO: Create a PyScript that returns the predictive Accuracy
  # # Change the number to 9999 as a "bogus number"
  # message("Fitting Machine Learning Models")
  # par <- reticulate::import('pycaret.parallel')
  # models <- reg$compare_models(n_select = as.integer(n_models))
  reticulate::source_python('R/scratch/test.py')
  Model <- reg$pull()$Model
  rmse <- vector(mode = "numeric", length = length(Model))

  pred_accuracy <- data.frame(Model, rmse)
  row.names(pred_accuracy) <- row.names(reg$pull())
  for (i in seq_len(length(models))) {
    pred_results <- reg$predict_model(models[[i]], data = test)
    pred_accuracy[i, 2] <- sqrt(sum((test$Y - pred_results$Label)^2) /
                                  nrow(test))
  }
  obj <- list(models = models, pred_accuracy = pred_accuracy)
  obj
  # Should we add a class so that when summary is called it displays nice?
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
