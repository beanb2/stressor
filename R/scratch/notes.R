# Get the benchmark data values with classification
# CV-K means
# Documentation of other pieces
# Start a vignette uses those two functions
#  Document in the vignette reference

# Ignoring Spatial Autocorrelation
library(tidyverse)
library(mlbench)
data("BostonHousing2")
boston <- select(.data = BostonHousing2, -town, -tract, -lon, -lat, -medv, -chas)
mlm_original <- compare_mlm(cmedv ~ ., data = boston)
boston_feat <- scale(select(.data = boston, -cmedv))
k_groups <- cluster_part2(boston_feat, 10)
pred_accuracy <- matrix(0, nrow = length(mlm_original$models),
                        ncol = max(k_groups))
for (i in seq_len(max(k_groups))) {
  test_index <- which(i == k_groups)
  test <- boston[test_index, ]
  train <- boston[-test_index, ]
  refitted <- refit_mlm(model = mlm_original, train, test)
  for (j in seq_len(length(refitted))){
    pred_accuracy[j, i] <- sqrt(sum((test$cmedv - refitted$Label)^2) /
                                  nrow(test))
  }
}
# perfect predictions because it has all of the data

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
  refitted <- refit_mlm(model = mlm_classification, train, test,
                        classification = TRUE)
  for (j in seq_len(length(refitted))){
    pred_accuracy[j, i] <- sum(test$Class == refitted$Label) / nrow(test)
  }
}
