# Asymptotic Example
# Matrix of weights
mat_asym <- matrix(c(1, 1.5, 1, 1, 1, 2, -1, 3, 3, 1), nrow = 2, ncol = 5)
data_asym <- data_gen_asym(1000, weight_mat = mat_asym, resp_sd = .1)

asym_model <- reg_asym(Y ~ ., data_asym, init_guess = c(rep(1, 10), 0))
asym_real <- reg_asym(Y ~ ., data_asym, init_guess = c(as.vector(mat_asym), 0))

create_virtualenv()
ml_models <- mlm_regressor(Y ~ ., data_asym)

# Cross Validation
asym_cv <- cv(asym_model, data_asym, n_folds = 10)
asym_rmse <- rmse(asym_cv, data_asym$Y)

asym_cvr <- cv(asym_real, data_asym, n_folds = 10)
asym_rmser <- rmse(asym_cvr, data_asym$Y)

ml_cv <- cv(ml_models, data_asym, n_folds = 10)
ml_rmse <- rmse(ml_cv, data_asym$Y)

# Spatial Cross Validation
asym_scv <- cv(asym_model, data_asym, n_folds = 10, k_mult = 5)
asym_scv_rmse <- rmse(asym_scv, data_asym$Y)

asym_scvr <- cv(asym_real, data_asym, n_folds = 10, k_mult = 5)
asym_scv_rmser <- rmse(asym_scvr, data_asym$Y)

ml_scv <- cv(ml_models, data_asym, n_folds = 10, k_mult = 5)
ml_scv_rmse <- rmse(ml_scv, data_asym$Y)

code <- c("asym", row.names(ml_models$pred_accuracy))
Models <- c("Asymptotic", ml_models$pred_accuracy$Model)
cv.rmse <- c(asym_rmse, ml_rmse$rmse)
scv.rmse <- c(asym_scv_rmse, ml_scv_rmse$rmse)
cv_df <- data.frame(code, Models, cv.rmse, scv.rmse)

# distance from center
distances <- dist_cent(Y ~ ., data_asym)

asym_dist <- data.frame(resids = residual(asym_cv, data_asym$Y),
                        dist = distances)
ml_resids <- residual(ml_cv, data_asym$Y)
ml_dist <- ml_resids
ml_dist$dist <- distances

models <- factor(rep(c("asym", row.names(ml_models$pred_accuracy)), each = 1000),
                 levels = c("asym", row.names(ml_models$pred_accuracy)))
dist <- rep(distances, 19)
resids <- c(residual(asym_cv, data_asym$Y), as.vector(as.matrix(ml_resids)))
dist_df <- data.frame(resids, dist, models)
dist_df_F <- dist_df[dist_df$models %in% c("asym", "gbr", "lightgbm", "rf"), ]
dist_df_F1 <- dist_df[dist_df$models %in% c("asym", "et", "dt", "ada"), ]
dist_df_F2 <- dist_df[dist_df$models %in% c("asym", "knn", "br", "ridge"), ]
dist_df_F3 <- dist_df[dist_df$models %in% c("asym", "lar", "lr", "huber"), ]
dist_df_F4 <- dist_df[dist_df$models %in% c("asym", "omp", "en", "lasso"), ]
dist_df_F5 <- dist_df[dist_df$models %in% c("asym", "llar", "dummy", "par"), ]

pdf("scripts/dist1.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(3, 18, 1),
                     limits = c(3, 18)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 2, 1),
                     limits = c(-5, 2)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist2.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F1, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(3, 18, 1),
                     limits = c(3, 18)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 2, 1),
                     limits = c(-5, 2)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist3.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F2, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(3, 18, 1),
                     limits = c(3, 18)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 2, 1),
                     limits = c(-5, 2)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist4.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F3, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(3, 18, 1),
                     limits = c(3, 18)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 2, 1),
                     limits = c(-5, 2)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist5.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F4, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(3, 18, 1),
                     limits = c(3, 18)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 2, 1),
                     limits = c(-5, 2)) +
  labs(color = "Models", pch = "Models")
dev.off()

pdf("scripts/dist6.pdf", width = 6, height = 4)
ggplot(mapping = aes(x = dist, y = resids)) +
  geom_point(aes(group = models), data = dist_df, color = "grey", alpha = .5) +
  geom_point(aes(group = models, color = models, pch = models), data = dist_df_F5, alpha = .7) +
  scale_shape_manual(values = c(16, 15, 17, 13))+
  scale_color_manual(values = new_palette) +
  scale_x_continuous(name = "Distance from Center", breaks = seq(3, 18, 1),
                     limits = c(3, 18)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 2, 1),
                     limits = c(-5, 2)) +
  labs(color = "Models", pch = "Models")
dev.off()

# Data thinning
asym_thin <- thinning(asym_model, data_asym)
size <- seq(.05, .95, .05)
asym_thin_rmse <- asym_thin$RMSE

mlm_thin <- thinning(ml_models, data_asym)
mlm_thin_rmse <- mlm_thin$RMSE

RMSE <- c(asym_thin_rmse, as.vector(mlm_thin_rmse))
thin_amt <- rep(size, 19)
models <- factor(rep(c("asym", row.names(ml_models$pred_accuracy)), each = 19),
                 levels = c("asym", row.names(ml_models$pred_accuracy)))
thin_df <- data.frame(RMSE, thin_amt, models)
thin_df_F <- thin_df[thin_df$models %in% c("asym", "gbr", "lightgbm", "rf", "et"), ]

col_palette <- RColorBrewer::brewer.pal(12, "Paired")
new_palette <- col_palette[c(6, 2, 4, 10, 12)]
my_palette <- c(new_palette, rep("lightgray", 14))
new_palette1 <- col_palette[c(6, 2, 4, 8, 10, 12)]

pdf("scripts/synth_thin1.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
               linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 1.7, by = .1), limits = c(0, 1.7)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                      limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

thin_df_F2 <- thin_df[thin_df$models %in% c("asym", "dt", "ada", "knn", "br"),]
thin_df_F3 <- thin_df[thin_df$models %in% c("asym", "ridge", "lar", "lr", "huber", "omp"), ]
thin_df_F4 <- thin_df[thin_df$models %in% c("asym", "en", "lasso", "llar", "dummy", "par"), ]

pdf("scripts/synth_thin2.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F2, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F2, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 1.7, by = .1), limits = c(0, 1.7)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette) +
  labs(color = "Models")
dev.off()

pdf("scripts/synth_thin3.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F3, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F3, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 1.7, by = .1), limits = c(0, 1.7)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette1) +
  labs(color = "Models")
dev.off()

pdf("scripts/synth_thin4.pdf", width = 6, height = 4)
ggplot() +
  geom_line(aes(thin_amt, RMSE, group = models), data = thin_df, color = "grey",
            alpha = .7) +
  geom_line(aes(thin_amt, RMSE, group = models, color = models),
            data = thin_df_F4, linewidth = .75, alpha = .9) +
  geom_point(aes(thin_amt, RMSE, group = models, color = models),
             data = thin_df_F4, alpha = .9) +
  geom_abline(slope = 0, intercept = .1, color = "black",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(0, 1.7, by = .1), limits = c(0, 1.7)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette1) +
  labs(color = "Models")
dev.off()
