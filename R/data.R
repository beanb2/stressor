#' Boston Housing Data
#'
#' A subset of data from the Housing data for 506 census tracts of Boston from
#'  the 1970 Census.
#'
#' @format
#' A data frame with 506 rows and 13 columns:
#' \describe{
#'   \item{cmedv}{corrected median value of owner-occupied homes in USD 1000's}
#'   \item{crim}{per capita crime rate by town}
#'   \item{zn}{proportion of residential land zoned for lots over 25,000 sq.ft}
#'   \item{indus}{proportion of non-retail business acres per town}
#'   \item{nox}{nitric oxides concentration (parts per 10 million)}
#'   \item{rm}{average number of rooms per dwelling}
#'   \item{age}{proportion of owner-occupied units built prior to 1940}
#'   \item{dis}{weighted distances to five Boston employment centres}
#'   \item{rad}{index of accessibility to radial highways}
#'   \item{tax}{full-value property-tax rate per USD 10,000}
#'   \item{ptratio}{pupil-teacher ratio by town}
#'   \item{b}{$1000(B−0.63)^2$ where BB is the proportion of blacks by town}
#'   \item{lstat}{percentage of lower status of the population}
#' }
#' @name boston
#' @source mlbench package
"boston"

#' @title Cross Validation for Machine Learning Models on Boston Housing data
#' @description To save computational cost of compile time of vignette, this
#'   was computed earlier to demonstrate the results
#' @format
#' \describe{
#'  A data frame with 506 rows and 18 columns
#'   \item{et}{Extra Trees Regressor}
#'   \item{pred_accuracy}{The initial predictive accuracy of the models}
#' }
#' @name mlm_vignette_boston
"mlm_vignette_boston"

#' @title Machine Learning Models fitted to Boston data
#' @description Fitted to a linear data set in this package to save computational time for
#'  the vignette. This data set was created by `"data_gen_lm()"` function in
#'  this package
#' @format
#' \describe{
#'   \item{models}{List of the models currently supported in PyCaret}
#'   \item{pred_accuracy}{The initial predictive accuracy of the models}
#' }
#' @name mlm_vignette_lm
"mlm_vignette_lm"
