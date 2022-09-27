#' Fit Machine Learning Regressor Models
#'
#' Through the pycaret module from python, this function fits many machine
#'   learning models simultaneously with little to no coding on the user
#'   part. The core function to fitting the initial models. This function is
#'   specifically designed for the classification models.
#' @param formula The classification formula, as a formula object
#' @param data A data frame object that includes the test data
#' @param fit_models A character vector with all the possible Machine Learning
#'   classifiers that are currently being fit, the user may specify a subset of
#'   them using a character vector.
#'   \tabular{rl}{
#'     ada \tab AdaBoost Classifier \cr
#'     et \tab Extra Trees Classifier \cr
#'     lightgbm \tab Light Gradient Boosting Machine\cr
#'     gbr \tab Gradient Boosting Classifier\cr
#'     lr \tab Logistic Regression\cr
#'     rf \tab Random Forest Classifier\cr
#'     ridge \tab Ridge Classifier\cr
#'     knn \tab K Neighbors Classifier\cr
#'     dt \tab Decision Tree Classifier\cr
#'     dummy \tab Dummy Classifier\cr
#'     svm \tab SVM - Linear Kernel\cr
#'     lda \tab Linear Discriminant Analysis\cr
#'     nb \tab Naive Bayes \cr
#'     qda \tab Quadratic Discriminant Analysis
#'   }
#' @param n_models An integer value defaulted to a large integer value to
#'   return all possible models.
#' @return A list object where the first entry is the models fitted and the
#'   second is the initial predictive accuracy on the random test data. Returns
#'   as two classes mlm_stressor and classifier
#' @export
mlm_classification <- function(formula, data,
                               fit_models = c('ada', 'et', 'lightgbm','dummy',
                                              'lr', 'rf', 'ridge', 'knn', 'dt',
                                              'gbr', 'svm', 'lda', 'nb', 'qda'),
                               n_models = 9999, ...) {
  obj <- mlm_init(formula, data, fit_models, n_models,
                  classification = TRUE, ...)
  class(obj) <- c(class(obj), "classifier")
  obj
}
