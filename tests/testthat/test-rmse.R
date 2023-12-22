test_that("rmse is calculated correctly", {
  obs <- c(1.1, 2.1, 3.0, 1.2, 1.4)
  vec1 <- c(1.123, 2.111, 3.001, 1.212, 1.414)
  vec2 <- c(0.993, 2.098, 2.994, 1.257, 1.509)
  vec3 <- c(1.101, 2.121, 3.111, 1.321, 1.225)
  pred_vec <- vec1
  pred_df <- data.frame(vec1, vec2, vec3)
  expect_equal(round(rmse(pred_vec, obs), 4), 0.0141)
  expect_equal(round(rmse(pred_df, obs)$rmse, 4), c(0.0141, 0.0730, 0.1077))
})
