# Data Thinning Visualization
library(ggplot2)
data <- data_gen_asym(1000, resp_sd = 1)
asym_model <- reg_asym(Y ~., data = data)
pred_thin_asym <- thinning(asym_model, data)$RMSE

data <- data_gen_lm(1000, weight_vec = c(1, 3, 4, 5, 7), resp_sd = 1)
lm_model <- lm(Y ~., data = data)
pred_thin_lm <- thinning(lm_model, data = data)$RMSE

rf_model <- randomForest::randomForest(Y ~ ., data = data)
pred_thin_rf <- thinning(rf_model, data = data)$RMSE

Models <- factor(rep(c("Asymptotic", "Random Forest"), each = 19),
                 levels = c("Asymptotic", "Random Forest"))
RMSE <- c(pred_thin_asym, pred_thin_rf)
thin_amt <- rep(seq(.05, .95, .05), 2)
g_df <- data.frame(RMSE, Models, thin_amt)

col_palette <- RColorBrewer::brewer.pal(8, "Dark2")
new_palette <- col_palette[c(1, 3, 8)]

pdf("scripts/Thinning_examp.pdf", width = 6, height = 4)
ggplot(data = g_df, aes(x = thin_amt, y = RMSE, color = Models)) +
  geom_line(linewidth = 1, alpha = .9) +
  geom_point() +
  geom_abline(slope = 0, intercept = 1, color = "red",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(.95, 1.3, by = .05), limits = c(.95, 1.3)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette)
dev.off()

# Distance vs Residuals
data_asym <- data_gen_asym(1000, resp_sd = 1)
asym_model <- reg_asym(Y ~., data = data_asym)
dist_asym <- dist_cent(Y ~., data_asym)
cv_asym <- cv(asym_model, data = data_asym)
resids_asym <- residual(cv_asym, data_asym$Y)
asym_df <- data.frame(dist_asym, resids)

pdf("scripts/asym_dist.pdf", width = 2.75, height = 3.25)
ggplot(data = asym_df, aes(x = dist_asym, y = resids)) +
  geom_point() +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 4, 1),
                     limits = c(0, 4)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-5, 4, 1),
                     limits = c(-5, 4))
dev.off()

data_lm <- data_gen_lm(1000, weight_vec = c(1, 3, 4, 5, 7), resp_sd = 1)
lm_model <- lm(Y ~., data = data_lm)
dist_lm <- dist_cent(Y ~., data_lm)
cv_lm <- cv(lm_model, data = data_lm)
resids_lm <- residual(cv_asym, data_lm$Y)
lm_df <- data.frame(dist_lm, resids_lm)

pdf("scripts/lm_dist.pdf", width = 2.75, height = 3.25)
ggplot(data = lm_df, aes(x = dist_lm, y = resids_lm)) +
  geom_point() +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  scale_y_continuous(name = "Residuals", breaks = seq(-30, 40, 5),
                     limits = c(-31, 40))
dev.off()

library(ggh4x)

scales_y <- list(
  Method == "Asymptotic" ~ scale_y_continuous(breaks = seq(-6, 4, 2),
                                                 limits = c(-6, 4)),
  Method == "Linear" ~ scale_y_continuous(breaks = seq(-30, 30, 10),
                                             limits = c(-32, 32))
)
residuals <- c(resids_asym, resids_lm)
distances <- c(dist_asym, dist_lm)
Method <- factor(rep(c("Asymptotic", "Linear"), each = 1000),
                 levels = c("Asymptotic", "Linear"))
joint_df <- data.frame(residuals, distances, Method)

pdf("scripts/jnt_dist.pdf", width = 6, height = 4)
ggplot(data = joint_df, aes(x = distances, y = residuals)) +
  geom_point() +
  facet_wrap(~ Method, scales = "free_y") +
  scale_x_continuous(name = "Distance from Center", breaks = seq(0, 5, 1),
                     limits = c(0, 5)) +
  facetted_pos_scales(y = scales_y) +
  ylab("Residuals")
dev.off()
