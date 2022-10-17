#' Compare Machine Learning Models
#'
#' Through the pycaret module from python, this function fits many machine
#'   learning models simultaneously with little to no coding on the user
#'   part. The core function to fitting the initial models.
#' @param formula The regression formula or classification formula
#' @param data A dataframe object that includes the test data
#' @param fit_models A character vector with all the possible Machine Learning
#'   regressors that are currently being fit, the user may specify a subset of
#'   them using a character vector.
#'   \tabular{rl}{
#'     ada \tab AdaBoost Regressor \cr
#'     br \tab Bayesian Ridge \cr
#'     dt \tab Decision Tree Regressor \cr
#'     dummy \tab Dummy Regressor \cr
#'     en \tab Elastic Net \cr
#'     et \tab Extra Trees Regressor \cr
#'     gbr \tab Gradient Boosting Regressor \cr
#'     huber \tab Huber Regressor \cr
#'     knn \tab K Neighbors Regressor \cr
#'     lar \tab Least Angle Regression \cr
#'     lasso \tab Lasso Regression \cr
#'     lightgbm \tab Light Gradient Boosting Machine \cr
#'     llar \tab Lasso Least Angle Regression \cr
#'     lr \tab Linear Regression \cr
#'     omp \tab Orthogonal Matching Pursuit \cr
#'     par \tab Passive Aggressive Regressor \cr
#'     rf \tab Random Forest Regressor\cr
#'     ridge \tab Ridge Regression
#'   }
#'  If classification is set to TRUE, these models can be used depending on user
#'  specified but these are the default values for classification:
#'   \tabular{rl}{
#'     ada \tab AdaBoost Classifier \cr
#'     dt \tab Decision Tree Classifier\cr
#'     dummy \tab Dummy Classifier\cr
#'     et \tab Extra Trees Classifier \cr
#'     gbr \tab Gradient Boosting Classifier\cr
#'     knn \tab K Neighbors Classifier\cr
#'     lda \tab Linear Discriminant Analysis\cr
#'     lightgbm \tab Light Gradient Boosting Machine\cr
#'     lr \tab Logistic Regression\cr
#'     nb \tab Naive Bayes \cr
#'     qda \tab Quadratic Discriminant Analysis\cr
#'     rf \tab Random Forest Classifier\cr
#'     ridge \tab Ridge Classifier\cr
#'     svm \tab SVM - Linear Kernel
#'   }
#' @param n_models A defaulted integer to return the maximum number of models
#' @param classification A boolean value tag to indicate if classification
#'   methods should be used.
#' @param ... Additional arguments passed to the setup function in PyCaret
#' @return A list object that contains all the fitted models and the CV
#'   predictive accuracy.
#' @importFrom stats model.frame terms
mlm_init <- function(formula, data, fit_models, n_models = 9999,
                        classification = FALSE, ...) {
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
  #  Github reference from PyCaret
  message("Setting up the data for fitting models. Please press return key.")
  exp_reg <- reg$setup(data = data, target = rr, n_jobs = as.integer(1), ...)

  message("Fitting Machine Learning Models")
  # Through the fugue package there is a possibility to parallelization
  #  I will have to dive deeper into the documentation into how to do it.
  # par <- reticulate::import('pycaret.parallel')
  models <- reg$compare_models(include = fit_models,
                               n_select = as.integer(n_models))
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
  # Possibly change to a formula/data type
  attr(obj, "formula") <- terms(formula, data = data)
  class(obj) <- "mlm_stressor"
  obj
}
