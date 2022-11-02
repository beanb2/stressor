#' Boston Housing Data
#'
#' A subset of data from the Housing data for 506 census tracts of Boston from
#'  the 1970 Census. Original data set can be found in the
#'  \link[mlbench:BostonHousing2]{mlbench} package.
#'
#' @usage data(boston)
#' @format
#' A data.frame with 506 rows and 13 columns:
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

#' @title Machine Learning Models fitted to Boston data Cross Validated
#' @description Results from the `cv()` function on the Boston data set to save
#'  computational time during the compiling of the vignette.
#' @usage data(mlm_vignette_boston_cv)
#' @format
#'  A data.frame with 18 columns and 506 rows where each column represents a
#'  different machine learning model.
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

#' @title Machine Learning Models fitted to Boston data with Clustering
#' @description Results from the `cv()` function on the Boston data set, where
#'  the `k_mult` was set to 5 to indicate clustered cross validation with five
#'  groups, to save computational time during the compiling of the vignette.
#'  @usage data(mlm_vignette_boston_cluster)
#' @format
#'  A data.frame with 18 columns and 506 rows where each column represents a
#'  different machine learning model.
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
#' @name mlm_vignette_boston_cluster
"mlm_vignette_boston_cluster"

#' @title Test predictive accuracy of Machine Learning Models on Boston data
#' @description The results from the `mlm_regressor()` function where we saved
#'  the initial predictive accuracy from the object.
#' @usage data(mlm_vignette_boston_pred)
#' @format
#'  A data.frame with 2 columns and 18 rows where each row is a different
#'    machine learning model.
#' \describe{
#'  \item{Model}{Machine Learning Models}
#'  \item{rmse}{Root mean square error}
#' }
#' @name mlm_vignette_boston_pred
"mlm_vignette_boston_pred"
