#' Compare Machine Learning Models
#'
#' Through the pycaret module from python, this function fits many machine
#'   learning models simultaneously with little to no coding on the user
#'   part.
#' @param formula The regression formula or classification formula
#' @param data A dataframe object that includes the test data
#' @param n_models A defaulted integer to return the maximum number of models
#' @param classification A boolean value tag to indicate if classification
#'   methods should be used.
#' @param ... Additional arguments passed to the setup function in pycaret
#' @return A list object that contains all the fitted models and the CV
#'   predictive accuracy.
#'
# Make into two separate functions and have a common function
compare_mlm <- function(formula, data, n_models = 9999,
                        classification = FALSE, ...) {
  # This broke when I added the models to it
  temp <- model.frame(formula = formula, data = data)
  vv <- attr(terms(formula(temp)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name
  if (classification) {
    # fit_models = c('ada', 'et', 'lightgbm', 'gbr',
    #                'lr', 'rf', 'ridge', 'knn', 'dt',
    #                'dummy', 'svm', 'lda', 'nb', 'qda')
    message("Importing Pycaret Classification")
    reg <- reticulate::import("pycaret.classification")
  } else {
    message("Importing Pycaret Regression")
    reg <- reticulate::import("pycaret.regression")
  }
  data_test <- sample(nrow(data), .1 * nrow(data))
  test <- data[data_test, ]
  data <- data[-data_test, ]
  # Need to take it off the parallel process to run -
  #  Github reference from Pycaret
  message("Setting up the data for fitting models. Please press return key.")
  exp_reg <- reg$setup(data = data, target = rr, n_jobs = as.integer(1), ...)

  message("Fitting Machine Learning Models")
  # Through the fugue package there is a possibility to parallelization
  #  I will have to dive deeper into the documentation into how to do it.
  # par <- reticulate::import('pycaret.parallel')
  models <- reg$compare_models(n_select = as.integer(n_models))
  Model <- reg$pull()$Model

  if (classification) {
    accuracy <- vector(mode = "numeric", length = length(Model))
    pred_accuracy <- data.frame(Model, accuracy)
    row.names(pred_accuracy) <- row.names(reg$pull())
    for (i in seq_len(length(models))) {
      pred_results <- reg$predict_model(models[[i]], data = test)
      pred_accuracy[i, 2] <- sum(test[, rr] == pred_results$Label) /
                                     nrow(test)
    }
  } else {
    rmse <- vector(mode = "numeric", length = length(Model))
    pred_accuracy <- data.frame(Model, rmse)
    row.names(pred_accuracy) <- row.names(reg$pull())
    for (i in seq_len(length(models))) {
      pred_results <- reg$predict_model(models[[i]], data = test)
      pred_accuracy[i, 2] <- sqrt(sum((test[, rr] - pred_results$Label)^2) /
                                    nrow(test))
    }
  }
  obj <- list(models = models, pred_accuracy = pred_accuracy)
  obj
  # Should we add a class so that when summary is called it displays nice?
}

# Can Save models for future use, by saving the pipeline
#  Are we wanting the prediction part to be separate?
#  Do we want to have a function that tunes a model?

# fit_models = c('ada', 'et', 'lightgbm', 'gbr',
# 'lr', 'rf', 'ridge', 'knn', 'dt',
# 'dummy', 'lar', 'br', 'huber', 'omp',
# 'lasso', 'en', 'llar', 'par')

# If set to TRUE, these models are used instead of
#   fit_models:
#   \tabular{rl}{
#     ada \tab AdaBoost Classifier \cr
#     et \tab Extra Trees Classifier \cr
#     lightgbm \tab Light Gradient Boosting Machine\cr
#     gbr \tab Gradient Boosting Classifier\cr
#     lr \tab Logistic Regression\cr
#     rf \tab Random Forest Classifier\cr
#     ridge \tab Ridge Classifier\cr
#     knn \tab K Neighbors Classifier\cr
#     dt \tab Decision Tree Classifier\cr
#     dummy \tab Dummy Classifier\cr
#     svm \tab SVM - Linear Kernel\cr
#     lda \tab Linear Discriminant Analysis\cr
#     nb \tab Naive Bayes \cr
#     qda \tab Quadratic Discriminant Analysis
#   }

# @param fit_models A character vector with all the possible Machine Learning
#   regressors
#   \tabular{rl}{
#     ada \tab AdaBoost Regressor \cr
#     et \tab Extra Trees Regressor \cr
#     lightgbm \tab Light Gradient Boosting Machine\cr
#     gbr \tab Gradient Boosting Regressor\cr
#     lr \tab Linear Regression\cr
#     rf \tab Random Forest Regressor\cr
#     ridge \tab Ridge Regression\cr
#     knn \tab K Neighbors Regressor\cr
#     dt \tab Decision Tree Regressor\cr
#     dummy \tab Dummy Regressor\cr
#     lar \tab Least Angle Regression\cr
#     br \tab Bayesian Ridge\cr
#     huber \tab Huber Regressor\cr
#    omp \tab Orthogonal Matching Pursuit\cr
#     lasso \tab Lasso Regression\cr
#     en \tab Elastic Net\cr
#     llar \tab Lasso Least Angle Regression\cr
#     par \tab Passive Aggressive Regressor
#   }
