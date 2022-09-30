stressor::create_virtualenv()
library(mlbench)
data("BostonHousing2")
boston <- dplyr::select(.data = BostonHousing2, -town, -tract, -lon, -lat,
                        -medv, -chas)
mlm_vignette_boston <- mlm_regressor(cmedv ~ ., boston)

data_lm <- data_gen_lm(500)
mlm_vignette_lm <- mlm_regressor(Y ~ ., data_lm)

usethis::use_data(boston)
usethis::use_data(mlm_vignette_boston)
usethis::use_data(mlm_vignette_lm)
