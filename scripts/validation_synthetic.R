library(ggplot2)
library(gridExtra)
library(grid)

simulation <- function(n, eps, weight_mat, reg_method, label, test_size = 100) {
  pred_accuracy <- matrix(0, nrow = length(eps), ncol = length(n))
  conv_mat <- matrix(0, nrow = length(eps), ncol = length(n))
  for (i in seq_len(ncol(pred_accuracy))) {
    for (j in seq_len(nrow(pred_accuracy))) {
      if (reg_method == "reg_sine") {
        temp <- data_gen_sine(n[i] + test_size, weight_mat = weight_mat,
                              resp_sd = eps[j])
        test <- sample(nrow(temp), test_size)
        data <- temp[-test,]
        test <- temp[test, ]
        test_y <- test[, 1]
        test <- test[, -1]
        obj <- reg_sine(Y ~ ., data = data)
        conv_mat[j, i] <- obj$convergence
      } else if (reg_method == "reg_asym") {
        temp <- data_gen_asym(n[i] + test_size, weight_mat = weight_mat, resp_sd = eps[j])
        test <- sample(nrow(temp), test_size)
        data <- temp[-test,]
        test <- temp[test, ]
        test_y <- test[, 1]
        test <- test[, -1]
        obj <- reg_asym(Y ~ ., data = data)
        conv_mat[j, i] <- obj$convergence
      } else {
        temp <- data_gen_lm(n[i] + test_size, weight_mat = weight_mat, resp_sd = eps[j])
        test <- sample(nrow(temp), test_size)
        data <- temp[-test,]
        test <- temp[test, ]
        test_y <- test[, 1]
        test <- test[, -1]
        obj <- lm(Y ~ ., data = data)
      }
      pred <- predict(obj, test)
      pred_accuracy[j, i] <- sqrt(sum((test_y - pred)^2) / test_size)
    }
  }
  pred_accuracy <- as.data.frame(pred_accuracy)
  colnames(pred_accuracy) <- label
  sim_list <- list(pred_accuracy, conv_mat)
  return(sim_list)
}

simulation2 <- function(n, eps, weight_mat, reg_method, test_size = 100,
                        seed = 43421) {
  pred_accuracy <- matrix(0, nrow = length(eps), ncol = length(n))
  set.seed(seed)
  for (i in seq_len(ncol(pred_accuracy))) {
    for (j in seq_len(nrow(pred_accuracy))) {
      if (reg_method == "reg_sine") {
        data <- data_gen_sine(n[i], weight_mat = weight_mat,
                              resp_sd = eps[j])
        test <- data_gen_sine(test_size, weight_mat = weight_mat,
                              resp_sd = eps[j])
        test_y <- test[, 1]
        test <- test[, -1]
        obj <- reg_sine(Y ~ ., data = data)
      } else if (reg_method == "reg_asym") {
        data <- data_gen_asym(n[i], weight_mat = weight_mat,
                              resp_sd = eps[j])
        test <- data_gen_asym(test_size, weight_mat = weight_mat,
                              resp_sd = eps[j])
        test_y <- test[, 1]
        test <- test[, -1]
        obj <- reg_asym(Y ~ ., data = data)
      } else {
        data <- data_gen_lm(n[i], weight_mat = weight_mat,
                            resp_sd = eps[j])
        test <- data_gen_lm(test_size, weight_vec = weight_mat,
                            resp_sd = eps[j])
        test_y <- test[, 1]
        test <- test[, -1]
        obj <- lm(Y ~ ., data = data)
      }
      pred <- predict(obj, test)
      pred_accuracy[j, i] <- sqrt(sum((test_y - pred)^2) / test_size)
    }
  }
  pred_accuracy <- as.data.frame(pred_accuracy)
  colnames(pred_accuracy) <- c("n10", "n100", "n1000", "n5000",
                               "n7500", "n10000")
  pred_accuracy
}

# Sinusoidal Regression
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 10000) # add in 5000
n <- c()
mat_sine <- matrix(rep(c(1, 1, 0), 5), nrow = 3, ncol = 5)
sine_accuracy_res <- simulation(n, eps, mat_sine, "reg_sine", lab,
                                test_size = 300)
sine_accuracy <- sine_accuracy_res[[1]]
sine_results <- as.data.frame(eps)
rmse <- c(sine_accuracy$`n = 100`, sine_accuracy$`n = 1000`)
groups <- c(rep("n = 100", 10), rep("n = 1000", 10))
sine_results$groups <- factor(sine_results$groups, levels = lab)

# Asymptotic Regression
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 300, 500, 700, 900, 1000) # add in 5000
lab <- c("n = 100", "n = 1000")
mat_asym <- matrix(rep(1, 10), nrow = 2, ncol = 5)
asym_accuracy2 <- simulation(n, eps, mat_asym, "reg_asym", lab, test_size = 100)
asym_results <- asym_accuracy2[[1]]

eps2 <- rep(seq(.1, 1, .1), 6)
asym_results2 <- as.data.frame(eps2)
rmse <- c(asym_results$`n = 100`, asym_results$`n = 300`,
          asym_results$`n = 500`, asym_results$`n = 700`,
          asym_results$`n = 900`, asym_results$`n = 1000`)
asym_results2$rmse <- rmse
asym_results2$groups <- c(rep("n = 100", 10), rep("n = 300", 10),
                          rep("n = 500", 10), rep("n = 700", 10),
                          rep("n = 900", 10), rep("n = 1000", 10))
asym_results2$groups <- factor(asym_results2$groups, levels = lab)
# Linear model
weight_vec <- c(1, 3, 4, 5, 7)
lm_accuracy_res <- simulation(n, eps, weight_vec, "linear", lab, test_size = 300)
lm_accuracy <- lm_accuracy_res[[1]]

lm_results <- as.data.frame(eps2)
rmse <- c(lm_accuracy$`n = 100`, lm_accuracy$`n = 300`,
          lm_accuracy$`n = 500`, lm_accuracy$`n = 700`,
          lm_accuracy$`n = 900`, lm_accuracy$`n = 1000`)
lm_results$rmse <- rmse
lm_results$groups <- c(rep("n = 100", 10), rep("n = 300", 10),
                          rep("n = 500", 10), rep("n = 700", 10),
                          rep("n = 900", 10), rep("n = 1000", 10))
lm_results$groups <- factor(lm_results$groups, levels = lab)

################################################################################
#
# Creation of Data Frame Objects
#
################################################################################
eps <- rep(seq(.1, 1, .1), 2)
groups <- factor(groups, levels = lab)

# Asymptotic
rmse <- c(asym_results$`n = 100`, asym_results$`n = 1000`)
as_df <- data.frame(rmse, eps, groups)

# Sinusoidal
rmse <- log(c(sine_accuracy$`n = 100`, sine_accuracy$`n = 1000`), 10)
sine_df <- data.frame(rmse, eps, groups)

# Linear
rmse <- c(lm_accuracy$`n = 100`, lm_accuracy$`n = 1000`)
lm_df <- data.frame(rmse, eps, groups)

# Joint
method <- factor(rep(c("Asymptotic", "Sinusoidal", "Linear"), each = 20),
                 levels = c("Asymptotic", "Sinusoidal", "Linear"))
rmse <- c(as_df$rmse, sine_df$rmse, lm_df$rmse)
eps <- rep(seq(.1, 1, .1), 6)
groups <- rep(groups, 3)
jt_df <- data.frame(rmse, eps, groups, method)
################################################################################
#
# Visualization
#
################################################################################
library(ggplot2)
library(grid)
library(gridExtra)
# Using ggh4x package to get facet scales to differ.
scales_y <- list(
  method == "Asymptotic" ~ scale_y_continuous(breaks = seq(0, 1.4, .2),
                                              limits = c(0, 1.4)),
  method == "Linear" ~ scale_y_continuous(breaks = seq(0, 1.2, .2),
                                          limits = c(0, 1.1)),
  method == "Sinusoidal" ~ scale_y_continuous(breaks = seq(0, 4, 1),
                                              limits = c(0, 4))
)
pdf("scripts/jnt_verification.pdf", width = 6, height = 4)
p <- ggplot(jt_df, aes(x = eps, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps, y = eps), color = "red") +
  facet_grid(rows = vars(method), cols = vars(groups), scales = "free_y") +
  #facet_wrap(method ~ groups, scales = "free_y", nrow = 3) +
  facetted_pos_scales(y = scales_y) +
  scale_x_continuous(breaks = seq(0, 1, by = .2), limits = c(0, 1)) +
  theme(axis.title=element_text(size=14,face="bold"))

g <- ggplotGrob(p)
yax <- which(g$layout$name == "ylab-l")
g[["grobs"]][[yax]]$children[[1]]$label <- c("RMSE", "log(RMSE)", "RMSE")
g[["grobs"]][[yax]]$children[[1]]$y <- grid::unit(seq(0.15, 0.85, length =3),
                                                  "npc")
grid.draw(g)
dev.off()

# Visualization for Spatial Cross Validation
library(RColorBrewer)
x1 <- rnorm(50)
x2 <- rnorm(50)
df <- data.frame(x1, x2)
groups <- cv_cluster(scale(as.matrix(df)), 5)
df$groups <- as.factor(groups)
pdf("scripts/presentation/scv_vis.pdf", width = 5, height = 3)
ggplot(df, aes(x = x1, y = x2, col = groups, pch = groups)) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set2")
dev.off()

library(randomForest)
# Random Forest Validation
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 1000) # add in 5000
# 100, 1000
# Quantify how hungry RF is in recovering linear dataset.
lab <- c("n = 100", "n = 1000")
n <- 10000
eps <- c(0.1, 1)
weight_vec <- c(1, 3, 4, 5, 7)
weight_vec <- c(1, 1, 1, 1, 1)
test_size <- 500
rf_pred_lm2 <- matrix(0, nrow = length(eps), ncol = length(n))
for (i in seq_len(length(n))) {
  for (j in seq_len(length(eps))) {
    all_lm_data <- data_gen_lm(n[i] + test_size, weight_vec, y_int = 0,
                               resp_sd = eps[j])
    index <- sample(seq_len(n[i] + test_size), n[i])
    lm_data <- all_lm_data[index, ]
    lm_test <- all_lm_data[-index, ]
    rf <- randomForest::randomForest(Y ~ ., data = lm_data)
    pred_rf <- predict(rf, newdata = lm_test)
    rf_pred_lm2[j, i] <- rmse(pred_rf, lm_test$Y)
  }
}

mat_sine <- matrix(rep(1, 15), nrow = 3, ncol = 5)

rf_pred_sine <- matrix(0, nrow = length(eps), ncol = length(n))
for (i in seq_len(length(n))) {
  for (j in seq_len(length(eps))) {
    all_lm_data <- data_gen_sine(n[i] + test_size, mat_sine, y_int = 0,
                               resp_sd = eps[j])
    index <- sample(seq_len(n[i] + test_size), n[i])
    lm_data <- all_lm_data[index, ]
    lm_test <- all_lm_data[-index, ]
    rf <- randomForest::randomForest(Y ~ ., data = lm_data)
    pred_rf <- predict(rf, newdata = lm_test)
    rf_pred_sine[j, i] <- rmse(pred_rf, lm_test$Y)
  }
}

mat_asym <- matrix(rep(1, 10), nrow = 2, ncol = 5)

rf_pred_asym <- matrix(0, nrow = length(eps), ncol = length(n))
for (i in seq_len(length(n))) {
  for (j in seq_len(length(eps))) {
    all_lm_data <- data_gen_asym(n[i] + test_size, mat_asym, y_int = 0,
                                 resp_sd = eps[j])
    index <- sample(seq_len(n[i] + test_size), n[i])
    lm_data <- all_lm_data[index, ]
    lm_test <- all_lm_data[-index, ]
    rf <- randomForest::randomForest(Y ~ ., data = lm_data)
    pred_rf <- predict(rf, newdata = lm_test)
    rf_pred_asym[j, i] <- rmse(pred_rf, lm_test$Y)
  }
}

eps <- rep(seq(.1, 1, by = .1), 6)
groups <- rep(c(rep("n = 100", 10), rep("n = 1000", 10)), 3)
data_from <- factor(rep(c("lm", "sine", "asym"), each = 20),
                    levels = c("asym", "sine","lm"),
                    labels = c("Asymptotic", "Sinusoidal", "Linear"))
rmse <- c(as.vector(rf_pred_lm), as.vector(rf_pred_sine),
          as.vector(rf_pred_asym))
rf_df <- data.frame(rmse, eps, groups, data_from)

scales_y_rf <- list(
  data_from == "Asymptotic" ~ scale_y_continuous(breaks = seq(0, 1.4, .2),
                                              limits = c(0, 1.4)),
  data_from == "Sinusoidal" ~ scale_y_continuous(breaks = seq(0, 1.4, .2),
                                          limits = c(0, 1.4)),
  data_from == "Linear" ~ scale_y_continuous(breaks = seq(0, 7, 1),
                                              limits = c(0, 7))
)
data_from_labs <- c("Asymptotic", "Sinusoidal", "Linear")
names(data_from_labs) <- c("asym", "sine", "lm")

pdf(file = "scripts/rf_verification.pdf", width = 6, height = 4)
ggplot(rf_df, aes(x = eps, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps, y = eps), color = "red") +
  facet_grid(rows = vars(data_from), cols = vars(groups), scales = "free_y") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2),
                     limits = c(0, 1)) +
  facetted_pos_scales(y = scales_y_rf) +
  scale_color_brewer(palette = "Dark2") +
  ylab("RMSE") +
  theme(axis.title=element_text(size=14,face="bold")) +
  guides(color=guide_legend(title="Data\nGeneration\nMethod"))
dev.off()
