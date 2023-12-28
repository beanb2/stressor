# Boston Example
tenv <- reticulate::virtualenv_list()
Sys.setenv(RETICULATE_PYTHON =
             paste0("C:/Users/14357/Documents/.virtualenvs/",
                    tenv[3],
                    "/Scripts/python.exe"))
reticulate::use_virtualenv(tenv[3])

stressor::create_virtualenv()
create_virtualenv()

set.seed(43421)
lm_data <- data_gen_lm(1000)
set.seed(43421)
indices <- split_data_prob(lm_data, .8)
train <- lm_data[indices, ]
test <- lm_data[!indices, ]
mlm_lm <- mlm_regressor(Y ~ ., train, sort_v = 'RMSE', seed = 43421)
mlm_pred <- predict(mlm_lm, test)
mlm_lm_cv <- cv(mlm_lm, lm_data, n_folds = 10)
mlm_pred_rmse <- rmse(mlm_pred, test$Y)

mlm_vignette <- list(pred_accuracy = mlm_lm$pred_accuracy, mlm_lm_cv = mlm_lm_cv)

#usethis::use_data(mlm_vignette, internal = TRUE)

saveRDS(mlm_vignette$pred_accuracy, file = "vignettes/pred_lm.rds")
saveRDS(mlm_vignette$mlm_lm_cv, file = "vignettes/mlm_lm_cv.rds")
saveRDS(mlm_pred_rmse, file = "vignettes/mlm_test.rds")

library(mlbench)
data("BostonHousing2")
boston <- dplyr::select(.data = BostonHousing2, -town, -tract, -lon, -lat,
                        -medv, -b)
boston$chas <- as.factor(boston$chas)
mlm_vignette_boston <- mlm_regressor(cmedv ~ ., boston, sort_v = 'RMSE')
mlm_vignette_boston_pred <- mlm_vignette_boston$pred_accuracy
mlm_vignette_boston_cv <- cv(mlm_vignette_boston, boston)
mlm_vignette_boston_cluster <- cv(mlm_vignette_boston, boston, n_folds = 10,
                                  k_mult = 5)

usethis::use_data(boston, overwrite = TRUE)
saveRDS(mlm_vignette_boston_pred, file = "vignettes/pred.rds")
saveRDS(mlm_vignette_boston_cv, file = "vignettes/cv.rds")
saveRDS(mlm_vignette_boston_cluster, file = "vignettes/cluster.rds")
# usethis::use_data(mlm_vignette_boston_pred, overwrite = TRUE)
# usethis::use_data(mlm_vignette_boston_cv, overwrite = TRUE)
# usethis::use_data(mlm_vignette_boston_cluster, overwrite = TRUE)
