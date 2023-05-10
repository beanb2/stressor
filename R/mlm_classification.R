#' Fit Machine Learning Classification Models
#'
#' Through the \href{https://pycaret.gitbook.io/docs/get-started/quickstart#classification}{PyCaret}
#'   module from python, this function fits many machine
#'   learning models simultaneously with without requiring any python
#'   programming on the part of the user. The core function to fitting the
#'   initial models. This function is specifically designed for the
#'   classification models fitted by PyCaret.
#' @param formula The classification formula, as a formula object
#' @param data A data frame object that includes the test data
#' @param fit_models A character vector with all the possible Machine Learning
#'   classifiers that are currently being fit, the user may specify a subset of
#'   them using a character vector.
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
#' @param n_models An integer value defaulted to a large integer value to
#'   return all possible models.
#' @param ... additional arguments passed onto \link[stressor]{mlm_init}
#' @return A list object where the first entry is the models fitted and the
#'   second is the initial predictive accuracy on the random test data. Returns
#'   as two classes `"mlm_stressor"` and `"classifier"`
#' @details
#'  PyCaret is a python module. Where machine learning models can be fitted with
#'   little coding by the user. The pipeline that PyCaret uses is that it has a
#'   setup function to parameterize the data that is easy for all the models to
#'   fit on. Then compare models function is executed which fits all the models
#'   that are currently available. This process takes less than five minutes for
#'   data.frame objects that are less than 10,000 rows.
#' @examples
#' \dontrun{
#'  lm_test <- data_gen_lm(20)
#'  binary_response <- sample(c(0, 1), 20, replace = TRUE)
#'  lm_test$Y <- binary_response
#'  mlm_class <- mlm_classification(Y ~ ., lm_test)
#' }
#' @export
mlm_classification <- function(formula, data,
                               fit_models = c('ada', 'et', 'lightgbm','dummy',
                                              'lr', 'rf', 'ridge', 'knn', 'dt',
                                              'gbc', 'svm', 'lda', 'nb', 'qda'),
                               n_models = 9999, ...) {
  obj <- mlm_init(formula, data, fit_models, n_models,
                  classification = TRUE, ...)
  class(obj) <- c(class(obj), "classifier")
  obj
}


