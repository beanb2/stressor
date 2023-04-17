# Linear Example
# Matrix of weights
vec_lm <- c(1, 3, 4, 5, 7)
data_lm <- data_gen_lm(1000, weight_vec = vec_lm, resp_sd = .1)

lm_model <- lm(Y ~ ., data = data_lm)


create_virtualenv()
ml_models <- mlm_regressor(Y ~ ., data_lm)

# Cross Validation
lm_cv <- cv(lm_model, data_lm, n_folds = 10)
lm_rmse <- rmse(lm_cv, data_lm$Y)

ml_cv <- cv(ml_models, data_lm, n_folds = 10)
ml_rmse <- rmse(ml_cv, data_lm$Y)

# Spatial Cross Validation
lm_scv <- cv(lm_model, data_lm, n_folds = 10, k_mult = 5)
lm_scv_rmse <- rmse(lm_scv, data_lm$Y)

ml_scv <- cv(ml_models, data_lm, n_folds = 10, k_mult = 5)
ml_scv_rmse <- rmse(ml_scv, data_lm$Y)

code <- c("lm", row.names(ml_models$pred_accuracy))
Models <- c("Linear", ml_models$pred_accuracy$Model)
cv.rmse <- c(lm_rmse, ml_rmse$rmse)
scv.rmse <- c(lm_scv_rmse, ml_scv_rmse$rmse)
cv_df <- data.frame(code, Models, cv.rmse, scv.rmse)

knitr::kable(cv_df, "latex")

# distance from center
distances <- dist_cent(Y ~ ., data_lm)

ml_resids <- residual(ml_cv, data_lm$Y)
ml_dist <- ml_resids
ml_dist$dist <- distances

models <- factor(rep(c("lm", row.names(ml_models$pred_accuracy)), each = 1000),
                 levels = c("lm", row.names(ml_models$pred_accuracy)))
dist <- rep(distances, 19)
resids <- c(residual(lm_cv, data_lm$Y), as.vector(as.matrix(ml_resids)))
dist_df <- data.frame(resids, dist, models)
dist_df_F <- dist_df[dist_df$models %in% c("lm", "lr", "lar", "br"), ]
dist_df_F1 <- dist_df[dist_df$models %in% c("lm", "huber", "ridge", "par"), ]
dist_df_F2 <- dist_df[dist_df$models %in% c("lm", "gbr", "lightgbm", "lasso"), ]
dist_df_F3 <- dist_df[dist_df$models %in% c("lm", "et", "rf", "knn"), ]
dist_df_F4 <- dist_df[dist_df$models %in% c("lm", "en", "ada", "dt"), ]
dist_df_F5 <- dist_df[dist_df$models %in% c("lm", "omp", "llar", "dummy"), ]

pdf("scripts/dist1_lm.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 35, 5),
                     limits = c(-30, 35)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist2_lm.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F1, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 35, 5),
                     limits = c(-30, 35)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist3_lm.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F2, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 35, 5),
                     limits = c(-30, 35)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist4_lm.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F3, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 35, 5),
                     limits = c(-30, 35)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist5_lm.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F4, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 35, 5),
                     limits = c(-30, 35)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist6_lm.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F5, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 35, 5),
                     limits = c(-30, 35)) +
  labs(color = "Models", pch = "Models")
dev.off()

# Data thinning
lm_thin <- thinning(lm_model, data_lm)
size <- seq(.05, .95, .05)
lm_thin_rmse <- lm_thin$RMSE

mlm_thin <- thinning(ml_models, data_lm)
mlm_thin_rmse <- mlm_thin$RMSE

RMSE <- c(lm_thin_rmse, as.vector(mlm_thin_rmse))
thin_amt <- rep(size, 19)
models <- factor(rep(c("lm", row.names(ml_models$pred_accuracy)), each = 19),
                 levels = c("lm", row.names(ml_models$pred_accuracy)))
thin_df <- data.frame(RMSE, thin_amt, models)
thin_df_F <- thin_df[thin_df$models %in% c("lm", "lr", "lar", "br", "huber"), ]

col_palette <- RColorBrewer::brewer.pal(12, "Paired")
new_palette <- col_palette[c(6, 2, 4, 10, 12)]
my_palette <- c(new_palette, rep("lightgray", 14))
new_palette1 <- col_palette[c(6, 2, 4, 8, 10, 12)]

pdf("scripts/synth_thin1_lm.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .5) +
  scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10.5)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

thin_df_F2 <- thin_df[thin_df$models %in% c("lm", "ridge", "par", "gbr", "lightgbm"),]
thin_df_F3 <- thin_df[thin_df$models %in% c("lm", "lasso", "et", "rf", "knn", "en"), ]
thin_df_F4 <- thin_df[thin_df$models %in% c("lm", "ada", "dt", "omp", "llar", "dummy"), ]

pdf("scripts/synth_thin2_lm.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F2, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F2, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10.5)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

pdf("scripts/synth_thin3_lm.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F3, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F3, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10.5)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette1) +
  labs(color = "Models")
dev.off()

pdf("scripts/synth_thin4_lm.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F4, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F4, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10.5)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette1) +
  labs(color = "Models")
dev.off()
