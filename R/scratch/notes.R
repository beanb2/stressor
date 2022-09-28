# Get the benchmark data values with classification
# CV-K means
# Documentation of other pieces

# Fix the Fitting (done)
# Standard CV (done)
# K-means (done)
# Data Thinning by 80/20 and digress (Random Sampling) (done)

# Distance from center of train (average of X variables) to create a plot of
#  residuals vs distance from center
#  1. Calculate the distance of the points from the center,

# Start a vignette uses those two functions
#  Document in the vignette reference

# Ignoring Spatial Autocorrelation
library(tidyverse)
library(mlbench)
data("BostonHousing2")
boston <- select(.data = BostonHousing2, -town, -tract, -lon, -lat, -medv, -chas)
mlm_original <- compare_mlm(cmedv ~ ., data = boston)

accuracy <- function(refitted){
  pred_accuracy_vec <- rep(0, length = length(refitted))
  for (j in seq_len(length(refitted))){
    pred_accuracy_vec[j] <- sqrt(sum((refitted[[j]]$cmedv -
                                        refitted[[j]]$Label)^2) /
                                   nrow(refitted[[j]]))
  }
  pred_accuracy_vec
}


# Standard CV
n_folds <- 10
xvs <- rep(1:n_folds,length = nrow(boston))
xvs <- sample(xvs)
pred_accuracy_cv <- matrix(0, nrow = length(mlm_original$models),
                           ncol = n_folds)

# Return the overall average error/ return the predictions for each fold
#  calculate error after
for(i in 1:10){
  train <- boston[xvs!=i, ]
  test <- boston[xvs==i, ]
  refitted <- refit_mlm(mlm_original, train, test)
  pred_accuracy_cv[, i] <- (nrow(test) / nrow(boston)) * accuracy(refitted)
}
rmse <- rowSums(pred_accuracy_cv)
pred_accuracy_cv < data.frame(rmse, pred_accuracy_cv,
                                 row.names = row.names(mlm_original$pred_accuracy))
row.names(pred_accuracy_cv) <- row.names(mlm_original$pred_accuracy)
pred_accuracy_cv

# K-means
boston_feat <- scale(select(.data = boston, -cmedv))
k_groups <- cluster_part2(boston_feat, 10)
pred_accuracy_kmeans <- matrix(0, nrow = length(mlm_original$models),
                               ncol = max(k_groups))

for (i in seq_len(max(k_groups))) {
  test_index <- which(i == k_groups)
  test <- boston[test_index, ]
  train <- boston[-test_index, ]
  refitted <- refit_mlm(mlm_original, train, test)
  pred_accuracy_kmeans[, i] <- accuracy(refitted)
}
row.names(pred_accuracy_kmeans) <- row.names(mlm_original$pred_accuracy)
colnames(pred_accuracy_kmeans) <- seq(1, 10, 1)
pred_accuracy_kmeans

# Data Thinning 80/20 - 20/80 Train/Test
train_size <- seq(.8, .2, -.05)
pred_accuracy_thinning <- matrix(0, nrow = length(mlm_original$models),
                                 ncol = length(train_size))

for (i in seq_len(length(train_size))) {
  train_index <- sample(1:nrow(boston), train_size[i] * nrow(boston))
  test <- boston[-train_index, ]
  train <- boston[train_index, ]
  refitted <- refit_mlm(mlm_original, train, test)
  pred_accuracy_thinning[, i] <- accuracy(refitted)
}
row.names(pred_accuracy_thinning) <- row.names(mlm_original$pred_accuracy)
colnames(pred_accuracy_thinning) <- train_size
pred_accuracy_thinning

data("Sonar")
Class <- select(.data = Sonar, Class)
sonar <- select(.data = Sonar, -Class) %>%
  cbind(Class, .)
mlm_classification <- compare_mlm(Class ~ ., data = Sonar, classification = TRUE)
sonar_feat <- scale(select(.data = Sonar, -Class))
k_groups <- cluster_part2(sonar_feat, 10)
pred_accuracy <- matrix(0, nrow = length(mlm_classification$models),
                        ncol = max(k_groups))
for (i in seq_len(max(k_groups))) {
  test_index <- which(i == k_groups)
  test <- sonar[test_index, ]
  train <- sonar[-test_index, ]
  refitted <- refit_mlm(mlm_classification, train, test,
                        classification = TRUE)
  for (j in seq_len(length(refitted))){
    pred_accuracy[j, i] <- sum(test$Class == refitted[[j]]$Label) / nrow(test)
  }
}


# Meeting 9/7/2022
# Generic Cross Validation
# Run for Asymptotic
# Check sine in the single dimension
# Run a study with dimensionality, variance


# Meeting 9/28/2022
# Create Examples for core functions
# Vignette done by Tuesday Night

# Real dataset on alternative cross-validation -
#  benchmark on a real dataset
