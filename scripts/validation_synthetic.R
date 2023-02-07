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

g1 <- ggplot(sine_accuracy) +
  geom_point(aes(x = eps, y = n100)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g2 <- ggplot(sine_accuracy) +
  geom_point(aes(x = eps, y = n300)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g3 <- ggplot(sine_accuracy) +
  geom_point(aes(x = eps, y = n500)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g4 <- ggplot(sine_accuracy) +
  geom_point(aes(x = eps, y = n700)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g5 <- ggplot(sine_accuracy) +
  geom_point(aes(x = eps, y = n900)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g6 <- ggplot(sine_accuracy) +
  geom_point(aes(x = eps, y = n1000)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2)

# Asymptotic Regression
eps <- seq(from = .1, to = 1, by = .1)
n <- c(100, 300, 500, 700, 900, 1000) # add in 5000
lab <- c("n100", "n300", "n500", "n700",
  "n900", "n1000")
mat_asym <- matrix(rep(1, 10), nrow = 2, ncol = 5)
asym_accuracy2 <- simulation(n, eps, mat_asym, "reg_asym", lab, test_size = 100)
asym_results <- asym_accuracy2[[1]]


g1 <- ggplot(asym_results) +
  geom_point(aes(x = eps, y = n100)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g2 <- ggplot(asym_results) +
  geom_point(aes(x = eps, y = n300)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g3 <- ggplot(asym_results) +
  geom_point(aes(x = eps, y = n500)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g4 <- ggplot(asym_results) +
  geom_point(aes(x = eps, y = n700)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g5 <- ggplot(asym_results) +
  geom_point(aes(x = eps, y = n900)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g6 <- ggplot(asym_results) +
  geom_point(aes(x = eps, y = n1000)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
pdf(file = "scripts/asym_verification.pdf")
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2,
             top = textGrob("Asymptotic Regression"))
dev.off()
# lm Regression
eps <- seq(from = .1, to = 1, by = .1)
n <- c(10, 100, 1000, 5000, 10000) # add in 5000
mat_lm <- c(1, 3, 2, 7, 4)
lm_accuracy <- simulation(n, eps, mat_lm, "lm", lab, test_size = 300)
lm_accuracy <- lm_accuracy[[1]]

g1 <- ggplot(lm_accuracy) +
  geom_point(aes(x = eps, y = n100)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g2 <- ggplot(lm_accuracy) +
  geom_point(aes(x = eps, y = n300)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g3 <- ggplot(lm_accuracy) +
  geom_point(aes(x = eps, y = n500)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g4 <- ggplot(lm_accuracy) +
  geom_point(aes(x = eps, y = n700)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g5 <- ggplot(lm_accuracy) +
  geom_point(aes(x = eps, y = n900)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
g6 <- ggplot(lm_accuracy) +
  geom_point(aes(x = eps, y = n1000)) +
  geom_line(aes(x = eps, y = eps), color = "red") +
  scale_x_continuous(breaks = seq(.1, 1, by = .1), limits = c(0, 1.1))
pdf(file = "scripts/linear_verification.pdf")
grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2,
             top = textGrob("Linear Regression"))
dev.off()
