library(tidyverse)
# testing classification
apmc <- readRDS("data-raw/apmc.RDS")

apmc_imp <- sf::st_drop_geometry(apmc)

apmc_nogeom <- dplyr::select(apmc, -GEOID, -FID, -STATE_NAME, -STATE_FIPS,
                             -FREQ, -CROP, -PHASE1, -IRR_ZERO,
                             -T_USDA_TEX_CLASS, -T_TEXTURE, -IRR_50,
                             -ADD_PROP, -AWC_CLASS, -DRAINAGE, -REF_DEPTH,
                             -IRR) %>%
  sf::st_drop_geometry() %>%
  na.omit()
apmc_nogeom$AP <- factor(apmc_nogeom$AP, levels = c(0, 1))
apmc_10000 <- apmc_nogeom[sample(135714, 10000), ]
apmc_train <- apmc_nogeom[sample(135714, 500), ]

Sys.unsetenv("RETICULATE_PYTHON")
library(stressor)
create_virtualenv()
Sys.time()
mlm_apmc3 <- mlm_classification(AP ~ ., apmc_nogeom)
Sys.time() # 25 min

# CV and SCV
apmc_cv <- cv(mlm_apmc3, data = apmc_nogeom, n_folds = 10)
apmc_scv <- cv(mlm_apmc3, data = apmc_nogeom, n_folds = 10, k_mult = 5)

# Thinning
apmc_thin <- thinning(mlm_apmc3, apmc_nogeom, classification = TRUE)

# Regression
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

mlm_yield <- mlm_regressor(formula, corn_yield)

# CV and SCV
mlm_yield_cv <- cv(mlm_yield, corn_yield, 10)
mlm_yield_scv <- cv(mlm_yield, corn_yield, 10, 5)
mlm_yield_scv_latlon <- cv(mlm_yield, corn_yield, 10, 5,
                    grouping_formula = ~ lat + lon)

mlm_yield_thin <- thinning(mlm_yield, corn_yield)

save(apmc_cv, apmc_scv, file = "scripts/apmc_cv_scv.Rdata")
save(apmc_thin, file = "scripts/ apmc_thin.Rdata")
save(mlm_yield_thin, file = "scripts/yield_thin.Rdata")
