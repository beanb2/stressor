stressor::create_virtualenv()
library(mlbench)
data("BostonHousing2")
boston <- dplyr::select(.data = BostonHousing2, -town, -tract, -lon, -lat,
                        -medv, -chas)
mlm_vignette_boston <- mlm_regressor(cmedv ~ ., boston)
mlm_vignette_boston_cv <- cv(mlm_vignette_boston, boston)

usethis::use_data(boston)
usethis::use_data(mlm_vignette_boston_cv, overwrite = TRUE)
