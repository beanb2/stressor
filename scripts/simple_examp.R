library(ggplot2)
library(stressor)
# Linear
lm_data <- data_gen_lm(100, weight_vec = c(1), resp_sd = .1)
pdf("scripts/linear.pdf", width = 3, height = 3)
ggplot(data = lm_data, aes(x = V1, y = Y)) +
  geom_point() +
  scale_y_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3)) +
  scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3))
dev.off()

# Asymptotic
asym_data <- data_gen_asym(100, weight_mat = matrix(c(1, 1), ncol = 1),
                           resp_sd = .1)
pdf("scripts/asymptotic.pdf", width = 3, height = 3)
ggplot(data = asym_data, aes(x = V1, y = Y)) +
  geom_point() +
  scale_y_continuous(breaks = seq(-1, .5, .25), limits = c(-1, .5)) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0,12))
dev.off()

# Sinusoidal
sine_data <- data_gen_sine(100, weight_mat = matrix(c(1, 1, 1), ncol = 1),
                           resp_sd = .1)
pdf("scripts/sinusoidal.pdf", width = 3, height = 3)
ggplot(data = sine_data, aes(x = V1, y = Y)) +
  geom_point() +
  scale_y_continuous(breaks = seq(-1.5, 1.5, .5), limits = c(-1.5, 1.5)) +
  scale_x_continuous(breaks = seq(-3, 3, 1), limits = c(-3, 3))
dev.off()
