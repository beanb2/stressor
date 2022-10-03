#' Boston Housing Data
#'
#' A subset of data from the Housing data for 506 census tracts of Boston from
#'  the 1970 Census.
#'
#' @usage data(boston)
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

#' @title Machine Learning Models fitted to Boston data
#' @description Fitted to the Boston data set in this package to save
#'  computational time for the vignette.
#' @usage data(mlm_vignette_boston_cv)
#' @format
#' \describe{
#'   \item{ada}{AdaBoost Regressor}
#'   \item{br}{Bayesian Ridge}
#'   \item{dt}{Decision Tree Regressor}
#'   \item{dummy}{Dummy Regressor}
#'   \item{en}{Elastic Net}
#'   \item{et}{Extra Trees Regressor}
#'   \item{gbr}{Gradient Boosting Regressor}
#'   \item{huber}{Huber Regressor}
#'   \item{knn}{K Neighbors Regressor}
#'   \item{lar}{Least Angle Regression}
#'   \item{lasso}{Lasso Regression}
#'   \item{lightgbm}{Light Gradient Boosting Machine}
#'   \item{llar}{Lasso Least Angle Regression}
#'   \item{lr}{Linear Regression}
#'   \item{omp}{Orthogonal Matching Pursuit}
#'   \item{par}{Passive Aggressive Regressor}
#'   \item{rf}{Random Forest Regressor}
#'   \item{ridge}{Ridge Regression}
#' }
#' @name mlm_vignette_boston_cv
"mlm_vignette_boston_cv"
