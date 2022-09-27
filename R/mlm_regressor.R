#' Fit Machine Learning Regressor Models
#'
#' Through the pycaret module from python, this function fits many machine
#'   learning models simultaneously with little to no coding on the user
#'   part. The core function to fitting the initial models. This function is
#'   specifically designed for the regression models.
#' @param formula The regression formula, as a formula object
#' @param data A data frame object that includes the test data
#' @param fit_models A character vector with all the possible Machine Learning
#'   regressors that are currently being fit, the user may specify a subset of
#'   them using a character vector.
#'   \tabular{rl}{
#'     ada \tab AdaBoost Regressor \cr
#'     et \tab Extra Trees Regressor \cr
#'     lightgbm \tab Light Gradient Boosting Machine\cr
#'     gbr \tab Gradient Boosting Regressor\cr
#'     lr \tab Linear Regression\cr
#'     rf \tab Random Forest Regressor\cr
#'     ridge \tab Ridge Regression\cr
#'     knn \tab K Neighbors Regressor\cr
#'     dt \tab Decision Tree Regressor\cr
#'     dummy \tab Dummy Regressor\cr
#'     lar \tab Least Angle Regression\cr
#'     br \tab Bayesian Ridge\cr
#'     huber \tab Huber Regressor\cr
#'    omp \tab Orthogonal Matching Pursuit\cr
#'     lasso \tab Lasso Regression\cr
#'     en \tab Elastic Net\cr
#'     llar \tab Lasso Least Angle Regression\cr
#'     par \tab Passive Aggressive Regressor
#'   }
#' @param n_models An integer value defaulted to a large integer value to
#'   return all possible models.
#' @return A list object where the first entry is the models fitted and the
#'   second is the initial predictive accuracy on the random test data. Returns
#'   as two classes mlm_stressor and regressor
#' @export
mlm_regressor <- function(formula, data,
                          fit_models = c('ada', 'et', 'lightgbm','gbr',
                                         'lr', 'rf', 'ridge', 'knn', 'dt',
                                         'dummy', 'lar', 'br', 'huber', 'omp',
                                         'lasso', 'en', 'llar', 'par'),
                          n_models = 9999, ...) {
  # Throw in a data_check/formula_check
  obj <- mlm_init(formula, data, fit_models, n_models, ...)
  class(obj) <- c(class(obj), "regressor")
  obj
}
