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

Models <- factor(rep(c("Asymptotic", "Linear", "Random Forest"), each = 19),
                 levels = c("Asymptotic", "Linear", "Random Forest"))
RMSE <- c(pred_thin_asym, pred_thin_lm, pred_thin_rf)
thin_amt <- rep(seq(.05, .95, .05), 3)
g_df <- data.frame(RMSE, Models, thin_amt)

col_palette <- RColorBrewer::brewer.pal(8, "Dark2")
new_palette <- col_palette[c(1, 3, 8)]

pdf("scripts/Thinning_examp.pdf", width = 6, height = 4)
ggplot(data = g_df, aes(x = thin_amt, y = RMSE, color = Models)) +
  geom_line(linewidth = 1, alpha = .9) +
  geom_point() +
  geom_abline(slope = 0, intercept = 1, color = "red",
              linetype = "longdash", linewidth = .8, alpha = .8) +
  scale_y_continuous(breaks = seq(.9, 1.2, by = .05), limits = c(.9, 1.2)) +
  scale_x_continuous(name = "Thinning Amount (%)\n(Training Size)", breaks = seq(0, 1, .1),
                     limits = c(0, 1)) +
  scale_color_manual(values = new_palette)
dev.off()

# Distance vs Residuals
data <- data_gen_asym(1000, resp_sd = 1)
asym_model <- reg_asym(Y ~., data = data)
dist_asym <- dist_cent(Y ~., data)
cv_asym <- cv(asym_model, data = data)
resids <- residual(cv_asym, data$Y)
asym_df <- data.frame(dist_asym, resids)

ggplot(data = asym_df, aes(x = dist_asym, y = resids)) +
  geom_point()

data <- data_gen_lm(1000, weight_vec = c(1, 3, 4, 5, 7), resp_sd = 1)
lm_model <- lm(Y ~., data = data)
dist_lm <- dist_cent(Y ~., data)
cv_lm <- cv(lm_model, data = data)
resids_lm <- residual(cv_asym, data$Y)
lm_df <- data.frame(dist_lm, resids_lm)

ggplot(data = lm_df, aes(x = dist_lm, y = resids_lm)) +
  geom_point()

rf_model <- randomForest::randomForest(Y ~ ., data = data)
