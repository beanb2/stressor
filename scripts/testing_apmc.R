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

# Distance to Center
apmc_dist <- dist_cent(AP ~ ., apmc_nogeom)
apmc_residual <- residual(as.integer(as.matrix(apmc_cv)), as.integer(apmc_nogeom$AP))
# Cut on Distance and find the average PCC
breaks <- c(2.5, 5.216, 6.575, 7.971, 40.9)
apmc_cuts <- cut(apmc_dist, breaks = breaks, right = FALSE, labels = FALSE)
dist_df <- data.frame(apmc_dist, apmc_cv, AP = apmc_nogeom$AP, cuts = apmc_cuts)
df2 <- dist_df %>% group_by(cuts) %>%
  summarise(rfPCC = sum((rf == AP)) / length(AP) * 100,
            etPCC = sum((et == AP)) / length(AP) * 100,
            lightPCC = sum((lightgbm == AP)) / length(AP) * 100,
            gbcPCC = sum((gbc == AP)) / length(AP) * 100,
            adaPCC = sum((ada == AP)) / length(AP) * 100,
            ldaPCC = sum((lda == AP)) / length(AP) * 100,
            ridgePCC = sum((ridge == AP)) / length(AP) * 100,
            dtPCC = sum((dt == AP)) / length(AP) * 100,
            lrPCC = sum((lr == AP)) / length(AP) * 100,
            knnPCC = sum((knn == AP)) / length(AP) * 100,
            qdaPCC = sum((qda == AP)) / length(AP) * 100,
            nbPCC = sum((nb == AP)) / length(AP) * 100,
            svmPCC = sum((svm == AP)) / length(AP) * 100,
            dummyPCC = sum((dummy == AP)) / length(AP) * 100,
            .groups = 'drop') %>%
  as.data.frame()
PCC <- unlist(c(df2[1, ][-1], df2[2, ][-1], df2[3, ][-1], df2[4, ][-1]))
model <- rep(colnames(apmc_cv), 4)
quantile <- rep(c(.25, .5, .75, 1), each = 14)
df_dist <- data.frame(PCC, model, quantile)
df_dist_F <- df_dist[df_dist$model %in% c("rf", "et", "lightgbm", "gbc"),]

pdf("scripts/quantile_apmc.pdf", width = 6, height = 4)
ggplot(df_dist, aes(x = quantile, y = PCC)) +
  geom_point(aes(group = model), size = 2, color = "grey", alpha = .5) +
  geom_point(aes(group = model, color = model, pch = model), size = 3, alpha = .7,
             data = df_dist_F) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Quantile (Distance from Center)", breaks = seq(.25, 1, .25),
                     limits = c(.25, 1)) +
  scale_y_continuous(name = "Residuals", breaks = seq(40, 90, 5),
                     limits = c(40, 90)) +
  labs(color = "Models", pch = "Models")
dev.off()

# Visualizations

models <- c("Random Forest Classifier", "Extra Trees Classifier",
            "Light Gradient Boosting Machine", "Gradient Boosting Classifier",
            "AdaBoost Classifier", "Linear Discriminant Analysis",
            "Ridge Classifier", "Decision Tree Classifier",
            "Logistic Regression", "K Neighbors Classifier",
            "Quadratic Discriminant Analysis", "Naive Bayes",
            "SVM - Linear Kernel", "Dummy Classifier")
cv_pcc <- vector("numeric", length = ncol(apmc_cv))
for (i in seq_len(ncol(apmc_cv))) {
  cv_pcc[i] <- sum(apmc_nogeom$AP == apmc_cv[, i]) / nrow(apmc_nogeom)
}
scv.pcc <- vector("numeric", length = ncol(apmc_scv))
for (i in seq_len(ncol(apmc_scv))) {
  scv.pcc[i] <- sum(apmc_nogeom$AP == apmc_scv[, i]) / nrow(apmc_nogeom)
}
cv_pcc <- cv_pcc * 100
scv.pcc <- scv.pcc * 100
cv_df <- data.frame(code = colnames(apmc_cv), models, cv_pcc, scv.pcc)

knitr::kable(cv_df, "latex", digits = 2)

PCC <- as.vector(apmc_thin$PCC)
thin_amt <- rep(seq(.05, .95, .05), 14)
models <- factor(rep(colnames(apmc_cv), each = 19),
                 levels = colnames(apmc_cv))
thin_df <- data.frame(PCC, thin_amt, models)
thin_df_F <- thin_df[thin_df$models %in% c("rf", "et", "lightgbm", "gbc", "ada"),]

col_palette <- RColorBrewer::brewer.pal(12, "Paired")
new_palette <- col_palette[c(6, 2, 4, 10, 12)]

pdf("scripts/apmc_thin.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, PCC, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, PCC, group = models, color = models),
            data = thin_df_F, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, PCC, group = models, color = models),
             data = thin_df_F, alpha = .9) +
  scale_y_continuous(breaks = seq(0.45, .85, by = .1), limits = c(.45, .85)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

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

yield_dist <- dist_cent(formula, corn_yield)

model_reg <- ml_models$pred_accuracy$Model
models.cv <- model_reg[c(3, 4, 2, 1, 7, 5, 6, 8, 9, 11, 15, 10, 14, 12, 13, 16, 17, 18)]
yield.cv <- rmse(mlm_yield_cv, corn_yield$YIELD)[, 2]
yield.scv <- rmse(mlm_yield_scv, corn_yield$YIELD)[, 2]
yield.scv.lat <- rmse(mlm_yield_scv_latlon, corn_yield$YIELD)[, 2]

cv_df <- data.frame(code = colnames(mlm_yield_cv), models.cv, yield.cv, yield.scv,
                    yield.scv.lat)

knitr::kable(cv_df, "latex", digits = 2)

save(apmc_cv, apmc_scv, file = "scripts/apmc_cv_scv.Rdata")
save(apmc_thin, file = "scripts/ apmc_thin.Rdata")
save(mlm_yield_thin, file = "scripts/yield_thin.Rdata")
save(mlm_yield_cv, mlm_yield_scv, mlm_yield_scv_latlon, file = "scripts/yield_cv.Rdata")
