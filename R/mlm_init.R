#' Compare Machine Learning Models
#'
#' Through the \href{https://pycaret.gitbook.io/docs/get-started/quickstart}{PyCaret}
#'   module from python, this function fits many machine
#'   learning models simultaneously with without requiring any python
#'   programming on the part of the user. The core function to fitting the
#'   initial models. This function is the backbone to fitting all the models.
#' @param formula The regression formula or classification formula. This formula
#'  should be linear.
#' @param data A data.frame object that includes the test data
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
#'  If classification is set to `TRUE`, these models can be used depending on user
#'  specified but these are the default values for classification:
#'   \tabular{rl}{
#'     ada \tab AdaBoost Classifier \cr
#'     dt \tab Decision Tree Classifier\cr
#'     dummy \tab Dummy Classifier\cr
#'     et \tab Extra Trees Classifier \cr
#'     gbc \tab Gradient Boosting Classifier\cr
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
#' @param classification A Boolean value tag to indicate if classification
#'   methods should be used.
#' @param seed An integer value to set the seed of the python environment.
#'   Default value is set to `NULL`.
#' @param ... Additional arguments passed to the setup function in PyCaret
#' @return A list object that contains all the fitted models and the CV
#'   predictive accuracy. With a class attribute of `"mlm_stressor"`
#' @details The formula should be linear, however, that does not imply a linear
#'   fit. The formula is a simply a convenient way to separate predictor
#'   variables from explanatory variables.
#'
#'   PyCaret is a python module. Where machine learning models can be fitted with
#'   little coding by the user. The pipeline that PyCaret uses is that it has a
#'   setup function to parameterize the data that is easy for all the models to
#'   fit on. Then compare models function is executed which fits all the models
#'   that are currently available. This process takes less than five minutes for
#'   data.frame objects that are less than 10,000 rows.
#' @inherit mlm_regressor examples
#' @importFrom stats model.frame terms
mlm_init <- function(formula, data, fit_models, n_models = 9999,
                        classification = FALSE, seed = NULL, ...) {
  # Declaring Constants
  regress_models <- c('ada', 'br', 'dt', 'dummy', 'en', 'et', 'gbr', 'huber',
                      'knn', 'lar', 'lasso', 'lightgbm', 'llar', 'lr', 'omp',
                      'par', 'rf', 'ridge')
  class_models <- c('ada', 'dt', 'dummy', 'et', 'gbc', 'knn', 'lda', 'lightgbm',
                    'lr', 'nb', 'qda', 'rf', 'ridge', 'svm')
  # Function input checks

  # Function starts here
  data <- model.frame(formula = formula, data = data)
  vv <- attr(terms(formula(data)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name
  if (classification) {
    sortv <- 'Accuracy'
    if (!all(is.element(fit_models, class_models))) {
      stop("The current models are not supported.")
    }
    message("Importing Pycaret Classification")
    reg <- reticulate::import("pycaret.classification")
  } else {
    sortv <- 'RMSE'
    if (!all(is.element(fit_models, regress_models))) {
      stop("The current models are not supported.")
    }
    message("Importing Pycaret Regression")
    reg <- reticulate::import("pycaret.regression")
  }
  data_test <- sample(nrow(data), .1 * nrow(data))
  test <- data[data_test, ]
  data <- data[-data_test, ]
  # Need to take it off the parallel process to run -
  #  Github reference from PyCaret
  message("Setting up the data for fitting models.")
  if (!is.null(seed)) {
    exp_reg <- reg$setup(data = data, target = rr, n_jobs = as.integer(1),
                         session_id = as.integer(seed), ...)
    reg$set_config('seed', as.integer(seed))
  } else {
    exp_reg <- reg$setup(data = data, target = rr, n_jobs = as.integer(1), ...)
  }

  message("Fitting Machine Learning Models")
  # Through the fugue package there is a possibility to parallelization
  #  I will have to dive deeper into the documentation into how to do it.
  # par <- reticulate::import('pycaret.parallel') include = fit_models,
  models <- reg$compare_models(include = fit_models,
                               sort = sortv,
                               n_select = as.integer(n_models))
  Model <- reg$pull()$Model

  if (classification) {
    accuracy <- vector(mode = "numeric", length = length(Model))
    pred_accuracy <- data.frame(Model, accuracy)
    row.names(pred_accuracy) <- row.names(reg$pull())
    for (i in seq_len(length(models))) {
      pred_results <- reg$predict_model(models[[i]], data = test)
      pred_values <- pred_results$prediction_label
      if (!all(suppressWarnings(is.na(as.integer(pred_values))))) {
        pred_results$prediction_label <- as.integer(pred_values)
      }
      pred_accuracy[i, 2] <- sum(test[, rr] == pred_results$prediction_label) /
                                     nrow(test)
    }
  } else {
    rmse <- vector(mode = "numeric", length = length(Model))
    pred_accuracy <- data.frame(Model, rmse)
    row.names(pred_accuracy) <- row.names(reg$pull())
    for (i in seq_len(length(models))) {
      pred_results <- reg$predict_model(models[[i]], data = test)
      pred_values <- pred_results$prediction_label
      pred_accuracy[i, 2] <- sqrt(sum((test[, rr] - pred_values)^2) /
                                    nrow(test))
    }
  }
  obj <- list(models = models, pred_accuracy = pred_accuracy)
  # Possibly change to a formula/data type
  attr(obj, "formula") <- terms(formula, data = data)
  class(obj) <- "mlm_stressor"
  obj
}
