#' Data and Formula helper check function
#'
#' Checks that the formula is a formula object, data is data.frame object, and
#'  if all the variables in the formula call are in the data.frame
#' @param formula A formula object
#' @param data A data.frame object
#' @param train_data A data.frame object. Some methods include two data.frames
#'  as inputs.
#' @returns Stops the function from continuing it's output by displaying an
#'  error message or nothing is returned.
data_check <- function(formula, data, train_data = NULL) {
  if (!inherits(formula, "formula")) {
    stop("The formula object must be of class formula and in the form
         'Y ~ x1 + x2 + ...'")
  }
  if (!is.null(train_data)){
    if (!all(inherits(data, "data.frame"),
             inherits(train_data, "data.frame"))) {
      stop("Data must be inheritted from data.frame or be a data.frame.")
    }
    if (!all(is.element(all.vars(attr(terms(formula), "variables")),
                        colnames(data)),
             is.element(all.vars(attr(terms(formula), "variables")),
                        colnames(train_data)))){
      stop("There are not matching variable names in the data.frame from the
         specified formula call.")
    }
  } else {
    if (!inherits(data, "data.frame")) {
      stop("Data must be inheritted from data.frame or be a data.frame.")
    }
    if (!all(is.element(all.vars(attr(terms(formula), "variables")),
                       colnames(data)))){
      stop("There are not matching variable names in the data.frame from the
           specified formula call.")
    }
  }
}
