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
