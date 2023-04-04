set.seed(43421)
vec1 <- rnorm(100)
vec2 <- rnorm(100)
mat <- matrix(vec1, vec2, nrow = 100, ncol = 2)
plot(mat)

clus <- cv_cluster(mat, 5, 5)
mat2 <- cbind(mat, clus)
matdf <- as.data.frame(mat2)
matdf$clus <- factor(matdf$clus)
ggplot(matdf, aes(x = V1, y=V2, col=clus)) +
  geom_point()


x <- rnorm(1000)
y <- x + rnorm(1000, sd = .1)
plot(x,y)
tdf <- data.frame(x = x, y = y)
test <- randomForest::randomForest(y~x)
