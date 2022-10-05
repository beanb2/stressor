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

usethis::use_data(boston)
usethis::use_data(mlm_vignette_boston_pred, overwrite = TRUE)
usethis::use_data(mlm_vignette_boston_cv, overwrite = TRUE)
usethis::use_data(mlm_vignette_boston_cluster, overwrite = TRUE)
