# Script to run 50 evaluations of CV and SCV
library(foreach)
library(doParallel)
total_cores <- detectCores()
cluster <- makeCluster(total_cores[1] / 2)
registerDoParallel(cluster)
validation_rep <- function(formula, data, n_folds = 10, k_mult = NULL,
                           repl = FALSE, grouping_formula = NULL, times = 10) {
  rmse_list <- vector(mode = "list", length = times)
  data <- model.frame(formula = formula, data = data)
  vv <- attr(terms(formula(data)), which = "variables")
  rr <- as.character(vv[[2]]) # The response variable name
  for (i in  1:times) {
    mlm_object <- mlm_regressor(formula, data, example = TRUE)
    cv_method <- cv(mlm_object, data, n_folds, k_mult, repl, grouping_formula)
    rmse_list[[i]] <- rmse(cv_method, data[, rr])
  }
  rmse_list
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
blue <- validation_rep(formula, corn_yield, times = 5)
red <- validation_rep(formula, corn_yield, n_folds = 10, k_mult = 5,
                                  times = 5)
green <- validation_rep(formula, corn_yield, n_folds = 10, k_mult = 5,
                        grouping_formula = ~ lat + lon, times = 5)

data_cv <- merge(blue[[1]], blue[[2]], by = "models") %>%
  merge(., blue[[3]], by = "models") %>%
  merge(., blue[[4]], by = "models") %>%
  merge(., blue[[5]], by = "models")
data_cv2 <- bind_rows(blue)
data_cv2$models <- as.factor(data_cv2$models)
data_cv2$Method <- rep("CV", nrow(data_cv2))

colnames(data_cv) <- c("models", "V1", "V2", "V3", "V4", "V5")

data_scv <- merge(red[[1]], red[[2]], by = "models") %>%
  merge(., red[[3]], by = "models") %>%
  merge(., red[[4]], by = "models") %>%
  merge(., red[[5]], by = "models")

data_scv2 <- bind_rows(red)
data_scv2$models <- as.factor(data_scv2$models)
data_scv2$Method <- rep("SCV", nrow(data_scv2))

data_joint <- bind_rows(data_cv2, data_scv2, data_scv_latlon)

colnames(data_scv) <- c("models", "V1", "V2", "V3", "V4", "V5")

pdf("scripts/cv5.pdf")
ggplot(data_cv2, aes(x = models, y = rmse)) +
  geom_boxplot(fill = "#F8766D", notch = FALSE) +
  ggtitle("Repeated 10-fold CV")
dev.off()

pdf("scripts/scv5.pdf")
ggplot(data_scv2, aes(x = models, y = rmse)) +
  geom_boxplot(fill = "#00BA38", notch = FALSE) +
  ggtitle("Repeated 10-fold SCV")
dev.off()

pdf("scripts/scv5_latlon.pdf")
ggplot(data_scv_latlon, aes(x = models, y = rmse)) +
  geom_boxplot(fill = "#619CFF") +
  ggtitle("Repeated 10-fold SCV with Grouping")
dev.off()

pdf("scripts/joint_cv.pdf")
ggplot(data_joint, aes(x = models, y = rmse, fill = Method)) +
  geom_boxplot() +
  scale_y_continuous(name = "RMSE", breaks = seq(15, 60, 5),
                     limits = c(15, 60)) +
  ggtitle("Repeated 10-fold CV on Various Techniques")
dev.off()

mlm_yield <- mlm_regressor(formula, corn_yield)

it2 <- cv(mlm_yield, corn_yield, 10, 5,
          grouping_formula = ~ lat + lon)
it3 <- cv(mlm_yield, corn_yield, 10, 5,
          grouping_formula = ~ lat + lon)
it4 <- cv(mlm_yield, corn_yield, 10, 5,
          grouping_formula = ~ lat + lon)
it5 <- cv(mlm_yield, corn_yield, 10, 5,
          grouping_formula = ~ lat + lon)
rmse1 <- rmse(mlm_yield_scv, corn_yield$YIELD)
rmse2 <- rmse(it2, corn_yield$YIELD)
rmse3 <- rmse(it3, corn_yield$YIELD)
rmse4 <- rmse(it4, corn_yield$YIELD)
rmse5 <- rmse(it5, corn_yield$YIELD)

data_scv_latlon <- bind_rows(rmse1, rmse2, rmse3, rmse4, rmse5)
data_scv_latlon$models <- as.factor(data_scv_latlon$models)
data_scv_latlon$Method <- rep("SCV_latlon", nrow(data_scv_latlon))
