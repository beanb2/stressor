#' @rdname predict
#' @title Prediction methods for various models
#' @description Predict values on `mlm_stressor`, `reg_asym`, or `reg_sine`
#'  objects. That expand the \link[stats]{predict()} function.
#' @param object A `mlm_stressor`, `reg_asym`, or `reg_sine` object
#' @param newdata A data.frame object that is the data to be predicted on
#' @param train_data A data.frame object defaulted to `NULL`, only used when
#'  an `mlm_stressor` object needs to be refitted.
#' @param ... Extending the \link[stats]{predict} function default, in this
#'  case it is ignored.
#' @return A data.frame of predictions if `mlm_stressor` object or vector of
#'  predicted values.
#' @name predict
NULL
