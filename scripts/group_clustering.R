set.seed(43421)
vec1 <- rnorm(100)
vec2 <- rnorm(100)
mat <- matrix(vec1, vec2, nrow = 100, ncol = 2)
plot(mat)

cv_groups <- create_groups(~ vec1 + vec2, as.data.frame(mat), n_folds = 5)
clus <- cv_cluster(mat, 5, 5)
mat2 <- cbind(mat, clus, cv_groups)
matdf <- as.data.frame(mat2)
matdf$clus <- factor(matdf$clus)
matdf$cv_groups <- factor(matdf$cv_groups)

pdf("scripts/scv_examp.pdf", width = 5, height = 3)
ggplot(matdf, aes(x = V1, y=V2, col=clus, pch = clus)) +
  geom_point(size = 3, alpha = .6) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-4, 3), limits = c(-4, 3)) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3), limits = c(-2, 3)) +
  labs(col = "Groups", pch = "Groups")
dev.off()

pdf("scripts/cv_examp.pdf", width = 5, height = 3)
ggplot(matdf, aes(x = V1, y = V2, col = cv_groups, pch = cv_groups)) +
  geom_point(size = 3, alpha = .6) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-4, 3), limits = c(-4, 3)) +
  scale_y_continuous(breaks = c(-2, -1, 0, 1, 2, 3), limits = c(-2, 3)) +
  labs(col = "Groups", pch = "Groups")
dev.off()

x <- rnorm(1000)
y <- x + rnorm(1000, sd = .1)
plot(x,y)
tdf <- data.frame(x = x, y = y)
test <- randomForest::randomForest(y~x)
