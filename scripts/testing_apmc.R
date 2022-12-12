# testing classification
apmc <- readRDS("C:/Users/14357/github/research/stressor/data-raw/apmc.RDS")
apmc_nogeom <- dplyr::select(apmc, -GEOID, -FID, -STATE_NAME, -STATE_FIPS, -FREQ,
                             -geometry, -CROP)
apmc_100 <- apmc_nogeom[sample(136620, 100), ]

Sys.unsetenv("RETICULATE_PYTHON")
library(stressor)
create_virtualenv()
Sys.time()
mlm_apmc <- mlm_classification(AP ~ ., apmc_100)
Sys.time()


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
data <- model.frame(formula, corn_yield)

x_data <- model.matrix(formula, corn_yield)[, -1]
x_data <- as.data.frame(x_data)

x_data_dist <- x_data[!duplicated(x_data[, c("lon", "lat")]), ]
x_data_features <- dplyr::select(x_data_dist, -lon, -lat)

groups <- stressor::cv_cluster(scale(x_data_features), 10, 5)
x_data_dist$groups <- groups

x_data_dist <- dplyr::select(x_data_dist, lon, lat, groups)

tryit <- dplyr::left_join(data, x_data_dist, by = c("lon", "lat"))

mlm_yield <- mlm_regressor(formula, corn_yield)
