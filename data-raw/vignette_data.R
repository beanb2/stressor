# Boston Example
stressor::create_virtualenv()
library(mlbench)
data("BostonHousing2")
boston <- dplyr::select(.data = BostonHousing2, -town, -tract, -lon, -lat,
                        -medv, -chas)
mlm_vignette_boston <- mlm_regressor(cmedv ~ ., boston)
mlm_vignette_boston_pred <- mlm_vignette_boston$pred_accuracy
mlm_vignette_boston_cv <- cv(mlm_vignette_boston, boston)
mlm_vignette_boston_cluster <- cv(mlm_vignette_boston, boston, n_folds = 10,
                                  k_mult = 5)

# Data Verification
simulation <- function(n, eps, weight_mat, label, test_size = 100,
                       seed = 43421) {
  pred_accuracy <- matrix(0, nrow = length(eps), ncol = length(n))
  conv_mat <- matrix(0, nrow = length(eps), ncol = length(n))
  set.seed(seed)
  for (i in seq_len(ncol(pred_accuracy))) {
    for (j in seq_len(nrow(pred_accuracy))) {
      temp <- data_gen_lm(n[i] + test_size, weight_mat = weight_mat,
                          resp_sd = eps[j])
      test <- sample(nrow(temp), test_size)
      data <- temp[-test,]
      test <- temp[test, ]
      test_y <- test[, 1]
      test <- test[, -1]
      obj <- lm(Y ~ ., data = data)
      pred <- predict(obj, test)
      pred_accuracy[j, i] <- sqrt(sum((test_y - pred)^2) / test_size)
    }
  }
  pred_accuracy <- as.data.frame(pred_accuracy)
  colnames(pred_accuracy) <- label
  sim_list <- list(pred_accuracy, conv_mat)
  return(sim_list)
}
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 300, 500, 700, 900, 1000) # add in 5000
lab <- c("n = 100", "n = 300", "n = 500", "n = 700",
         "n = 900", "n = 1000")
weight_vec <- c(1, 3, 4, 5, 7)
lm_accuracy_res <- simulation(n, eps, weight_vec, lab)
lm_accuracy <- lm_accuracy_res[[1]]

eps2 <- rep(seq(.1, 1, .1), 6)
lm_results <- as.data.frame(eps2)
rmse <- c(lm_accuracy$`n = 100`, lm_accuracy$`n = 300`,
          lm_accuracy$`n = 500`, lm_accuracy$`n = 700`,
          lm_accuracy$`n = 900`, lm_accuracy$`n = 1000`)
lm_results$rmse <- rmse
lm_results$groups <- c(rep("n = 100", 10), rep("n = 300", 10),
                       rep("n = 500", 10), rep("n = 700", 10),
                       rep("n = 900", 10), rep("n = 1000", 10))
lm_results$groups <- factor(lm_results$groups, levels = lab)

usethis::use_data(boston)
usethis::use_data(mlm_vignette_boston_pred, overwrite = TRUE)
usethis::use_data(mlm_vignette_boston_cv, overwrite = TRUE)
usethis::use_data(mlm_vignette_boston_cluster, overwrite = TRUE)
