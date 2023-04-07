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
