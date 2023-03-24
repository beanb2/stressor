library(ggplot2)
library(gridExtra)
library(grid)

simulation <- function(n, eps, weight_mat, reg_method, label, test_size = 100,
                       seed = 43421) {
  pred_accuracy <- matrix(0, nrow = length(eps), ncol = length(n))
  conv_mat <- matrix(0, nrow = length(eps), ncol = length(n))
  set.seed(seed)
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
eps <- seq(from = .5, to = 10, by = .5)
n <- c(10, 100, 1000, 5000, 7500, 10000) # add in 5000
n <- c()
mat_sine <- matrix(rep(1, 15), nrow = 3, ncol = 5)
sine_accuracy_res <- simulation(n, eps, mat_sine, "reg_sine", lab,
                                test_size = 500)
sine_accuracy <- sine_accuracy_res[[1]]
sine_results <- as.data.frame(eps2)
rmse <- c(sine_accuracy$`n = 100`, sine_accuracy$`n = 300`,
          sine_accuracy$`n = 500`, sine_accuracy$`n = 700`,
          sine_accuracy$`n = 900`, sine_accuracy$`n = 1000`)
sine_results$rmse <- rmse
sine_results$groups <- c(rep("n = 100", 10), rep("n = 300", 10),
                          rep("n = 500", 10), rep("n = 700", 10),
                          rep("n = 900", 10), rep("n = 1000", 10))
sine_results$groups <- factor(sine_results$groups, levels = lab)

# Asymptotic Regression
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 300, 500, 700, 900, 1000) # add in 5000
lab <- c("n = 100", "n = 300", "n = 500", "n = 700",
  "n = 900", "n = 1000")
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
lm_accuracy_res <- simulation(n, eps, weight_vec, "linear", lab)
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

pdf("scripts/asym_verification.pdf")
ggplot(asym_results2, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2))+
  facet_wrap(~ groups, nrow = 2) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

pdf("scripts/lm_verification.pdf")
ggplot(lm_results, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2)) +
  facet_wrap(~ groups, nrow = 2) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

pdf("scripts/sine_verification.pdf")
ggplot(sine_results, aes(x = eps2, y = log(rmse))) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  facet_wrap(~ groups, nrow = 2) +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 6.5, by = .5), limits = c(0, 6.5)) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

# Slide graphics

# Linear Models
lm_1 <- lm_results[1:30, ]
lm_2 <- lm_results[31:60, ]
pdf("scripts/presentation/lm_1.pdf")
ggplot(lm_1, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2)) +
  facet_wrap(~ groups, nrow = 1) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

pdf("scripts/presentation/lm_2.pdf")
ggplot(lm_2, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2)) +
  facet_wrap(~ groups, nrow = 1) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

# Asymptotic
asym_1 <- asym_results2[1:30, ]
asym_2 <- asym_results2[31:60, ]
pdf("scripts/presentation/asym_1.pdf")
ggplot(asym_1, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2)) +
  facet_wrap(~ groups, nrow = 1) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

pdf("scripts/presentation/asym_2.pdf")
ggplot(asym_2, aes(x = eps2, y = rmse)) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 1.2, by = .2), limits = c(0, 1.2)) +
  facet_wrap(~ groups, nrow = 1) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

# Sinusoidal
sine_1 <- sine_results[1:30, ]
sine_2 <- sine_results[31:60, ]
pdf("scripts/presentation/sine_1.pdf")
ggplot(sine_1, aes(x = eps2, y = log(rmse))) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  facet_wrap(~ groups, nrow = 1) +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 6.5, by = .5), limits = c(0, 6.5)) +
  theme(axis.title=element_text(size=14,face="bold"))
dev.off()

pdf("scripts/presentation/sine_2.pdf")
ggplot(sine_2, aes(x = eps2, y = log(rmse))) +
  geom_point() +
  geom_line(aes(x = eps2, y = eps2), color = "red") +
  facet_wrap(~ groups, nrow = 1) +
  scale_x_continuous(name = "eps", breaks = seq(0, 1, by = .2), limits = c(0.0, 1.05)) +
  scale_y_continuous(breaks = seq(0, 6.5, by = .5), limits = c(0, 6.5)) +
  theme(axis.title=element_text(size=14,face="bold"))
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
