# Get the benchmark data values with classification
# CV-K means
# Documentation of other pieces
# Start a vignette uses those two functions
#  Document in the vignette reference

# Ignoring Spatial Autocorrelation
library(tidyverse)
library(mlbench)
data("BostonHousing2")
boston <- select(.data = BostonHousing2, -town, -tract, -lon, -lat, -medv)
mlm_original <- compare_mlm(cmedv ~ ., data = boston)
boston_feat <- scale(select(.data = boston, -cmedv, -chas))
k_groups <- cluster_part2(boston_feat, 10)
for (i in seq_len(max(k_groups))) {
  test_index <- which(i == k_groups)
  test <- boston[test_index, ]
  train <- boston[-test_index, ]
  for (j in seq_len(length(mlm_original$models))){

  }
}
