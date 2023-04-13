# Sinusoidal Example
# Matrix of weights
mat_sine <- matrix(c(1, 1, 0, 2, -1, 1, 1, 1, 1, 3, 2, 1, -2, 1, 1), nrow = 3,
                   ncol = 5)
data_sine <- data_gen_sine(1000, weight_mat = mat_sine, resp_sd = .1)

sine_model <- reg_sine(Y ~ ., data_sine, init_guess = c(rep(1, 15), 0))

create_virtualenv()
ml_models <- mlm_regressor(Y ~ ., data_sine)

# Cross Validation
sine_cv <- cv(sine_model, data_sine, n_folds = 10)
sine_rmse <- rmse(sine_cv, data_sine$Y)

ml_cv <- cv(ml_models, data_sine, n_folds = 10)
ml_rmse <- rmse(ml_cv, data_sine$Y)

# Spatial Cross Validation
sine_scv <- cv(sine_model, data_sine, n_folds = 10, k_mult = 5)
sine_scv_rmse <- rmse(sine_scv, data_sine$Y)

ml_scv <- cv(ml_models, data_sine, n_folds = 10, k_mult = 5)
ml_scv_rmse <- rmse(ml_scv, data_sine$Y)

code <- c("sine", row.names(ml_models$pred_accuracy))
Models <- c("Sinusoidal", ml_models$pred_accuracy$Model)
cv.rmse <- c(sine_rmse, ml_rmse$rmse)
scv.rmse <- c(sine_scv_rmse, ml_scv_rmse$rmse)
cv_df <- data.frame(code, Models, cv.rmse, scv.rmse)

knitr::kable(cv_df, "latex")

# distance from center
distances <- dist_cent(Y ~ ., data_sine)

ml_resids <- residual(ml_cv, data_sine$Y)
ml_dist <- ml_resids
ml_dist$dist <- distances

models <- factor(rep(c("sine", row.names(ml_models$pred_accuracy)), each = 1000),
                 levels = c("sine", row.names(ml_models$pred_accuracy)))
dist <- rep(distances, 19)
resids <- c(residual(sine_cv, data_sine$Y), as.vector(as.matrix(ml_resids)))
dist_df <- data.frame(resids, dist, models)
dist_df <- dist_df[dist_df$models %in% row.names(ml_models$pred_accuracy), ]
dist_df_F <- dist_df[dist_df$models %in% c("gbr", "lightgbm", "et"), ]
dist_df_F1 <- dist_df[dist_df$models %in% c("rf", "ada", "dt"), ]
dist_df_F2 <- dist_df[dist_df$models %in% c("knn", "lr", "ridge"), ]
dist_df_F3 <- dist_df[dist_df$models %in% c("lar", "br", "huber"), ]
dist_df_F4 <- dist_df[dist_df$models %in% c("omp", "en", "dummy"), ]
dist_df_F5 <- dist_df[dist_df$models %in% c("lasso", "llar", "par"), ]

pdf("scripts/dist1_sine.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 9, 1),
                    limits = c(0, 9)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-15, 15, 1),
                    limits = c(-15, 15)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist2_sine.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F1, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 9, 1),
                     limits = c(0, 9)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-15, 15, 1),
                     limits = c(-15, 15)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist3_sine.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F2, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 9, 1),
                     limits = c(0, 9)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-15, 15, 1),
                     limits = c(-15, 15)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist4_sine.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F3, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 9, 1),
                     limits = c(0, 9)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-15, 15, 1),
                     limits = c(-15, 15)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist5_sine.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F4, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 9, 1),
                     limits = c(0, 9)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-15, 15, 1),
                     limits = c(-15, 15)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist6_sine.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F5, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 9, 1),
                     limits = c(0, 9)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-15, 15, 1),
                     limits = c(-15, 15)) +
  labs(color = "Models", pch = "Models")
dev.off()

# Data thinning
sine_thin <- thinning(sine_model, data_sine)
size <- seq(.05, .95, .05)
sine_thin_rmse <- sine_thin$RMSE

mlm_thin <- thinning(ml_models, data_sine)
mlm_thin_rmse <- mlm_thin$RMSE

RMSE <- as.vector(mlm_thin_rmse)
thin_amt <- rep(size, 18)
models <- factor(rep(c(row.names(ml_models$pred_accuracy)), each = 19),
                 levels = c(row.names(ml_models$pred_accuracy)))
thin_df <- data.frame(RMSE, thin_amt, models)
thin_df_F <- thin_df[thin_df$models %in% c("gbr", "lightgbm", "et", "rf"), ]

col_palette <- RColorBrewer::brewer.pal(12, "Paired")
new_palette <- col_palette[c(2, 4, 10, 12)]
my_palette <- c(new_palette, rep("lightgray", 14))
new_palette1 <- col_palette[c(2, 4, 8, 10, 12)]

pdf("scripts/synth_thin1_sine.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

thin_df_F2 <- thin_df[thin_df$models %in% c("ada", "dt", "knn", "lr"),]
thin_df_F3 <- thin_df[thin_df$models %in% c("ridge", "lar", "br", "huber", "omp"), ]
thin_df_F4 <- thin_df[thin_df$models %in% c("en", "dummy", "lasso", "llar", "par"), ]

pdf("scripts/synth_thin2_sine.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F2, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F2, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

pdf("scripts/synth_thin3_sine.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F3, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F3, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette1) +
  labs(color = "Models")
dev.off()

pdf("scripts/synth_thin4_sine.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F4, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F4, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette1) +
  labs(color = "Models")
dev.off()
