vec1 <- rnorm(100)
vec2 <- rnorm(100)
mat <- matrix(vec1, vec2, nrow = 100, ncol = 2)
plot(mat)

clus <- cluster_part2(mat, 5, 5)
mat2 <- cbind(mat, clus)
matdf <- as.data.frame(mat2)
ggplot(matdf, aes(x = V1, y=V2, col=clus)) +
  geom_point() +
  scale_colour_viridis_c()
