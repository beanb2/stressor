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
