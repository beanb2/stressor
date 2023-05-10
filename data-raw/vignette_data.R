# Boston Example
tenv <- reticulate::virtualenv_list()
Sys.setenv(RETICULATE_PYTHON =
             paste0("C:/Users/14357/Documents/.virtualenvs/",
                    tenv[3],
                    "/Scripts/python.exe"))
reticulate::use_virtualenv(tenv[3])

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

usethis::use_data(boston, overwrite = TRUE)
saveRDS(mlm_vignette_boston_pred, file = "vignettes/pred.rds")
saveRDS(mlm_vignette_boston_cv, file = "vignettes/cv.rds")
saveRDS(mlm_vignette_boston_cluster, file = "vignettes/cluster.rds")
# usethis::use_data(mlm_vignette_boston_pred, overwrite = TRUE)
# usethis::use_data(mlm_vignette_boston_cv, overwrite = TRUE)
# usethis::use_data(mlm_vignette_boston_cluster, overwrite = TRUE)
