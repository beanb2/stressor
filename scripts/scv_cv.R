# Script to run 50 evaluations of CV and SCV

validation_rep <- function(formula, data, n_folds = 10, k_mult = NULL,
                           repl = FALSE, grouping_formula = NULL, times = 10) {
  rmse_mat <- matrix(0, nrow = 18, ncol = times)
  data <- model.frame(formula = formula, data = data)
  vv <- attr(terms(formula(data)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name
  for (i in seq_len(times)) {
    mlm_object <- mlm_regressor(formula, data, example = TRUE)
    cv_method <- cv(mlm_object, data, n_folds, k_mult, repl, grouping_formula)
    rmse_mat[, i] <- rmse(cv_method[, i], data[, rr])
  }
  rmse_vec <- apply(rmse_mat, 2, mean)
  rmse_vec
}

library(tidyverse)
corn_yield <- readRDS('data-raw/true-train.RDS')


# Add coordinates to county data.
counties <- sf::st_read("data-raw/tl_2019_us_county/tl_2019_us_county.shp")
county_centroid <- sf::st_centroid(counties)
county_join <- county_centroid %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  as.data.frame(.) %>%
  dplyr::select(GEOID, lon, lat)
corn_yield <- dplyr::left_join(corn_yield, county_join, by = "GEOID")

formula <- YIELD ~
  YEAR +
  SDI_CDL_AG +
  SLOPE + ELEVATION +
  PERC_IRR + GDD + BV2 + BV4 + BV8 + BV9 + BV15 + BV18 + BV19 + TP +
  S_PH_H2O + T_CEC_SOIL + T_REF_BULK_DENSITY + T_OC

validation <- matrix(0, nrow =18, ncol = 3)
validation[, 1] <- validation_rep(formula, corn_yield, times = 50)
validation[, 2] <- validation_rep(formula, corn_yield, n_folds = 10, k_mult = 5,
                                  times = 50)
validation[, 3] <- validation_rep(formula, corn_yield, n_folds = 10, k_mult = 5,
                                  grouping_formula = ~ lat + lon, trials,
                                  times = 50)

