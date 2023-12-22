test_that("mlm_regressor fits regression models and is reproducible", {
  skip_if_no_python()
  set.seed(43421)
  lm_data <- data_gen_lm(25)
  mlm_regressor <- mlm_regressor(Y ~ ., lm_data, seed = 43421)
  initial_pred <- round(mlm_regressor$pred_accuracy[,2], 2)
  expect_equal(length(mlm_regressor), 2)
  expect_equal(length(mlm_regressor$models), 18)
  expect_equal(is.data.frame(mlm_regressor$pred_accuracy), TRUE)
  expect_s3_class(mlm_regressor, "mlm_stressor")
  expect_s3_class(mlm_regressor, "regressor")
  expect_equal(initial_pred, c(1.61, 1.98, 1.98, 1.97, 1.97, 2.70, 1.45, 1.28,
                               2.87, 2.61, 2.21, 2.48, 3.13, 2.75, 2.75, 4.40,
                               1.41, 1.41))
})

test_that("mlm_classification fits classification models and is reproducible", {
  skip_if_no_python()
  set.seed(43421)
  binary_resp <- sample(c(0, 1), 50, replace = TRUE)
  sine_class <- data_gen_sine(50)
  sine_class$Y <- binary_resp
  mlm_class <- mlm_classification(Y ~ ., sine_class, seed = 43421)
  initial_pred <- round(mlm_class$pred_accuracy[, 2], 2)
  expect_equal(length(mlm_class), 2)
  expect_equal(length(mlm_class$models), 14)
  expect_equal(is.data.frame(mlm_class$pred_accuracy), TRUE)
  expect_s3_class(mlm_class, "mlm_stressor")
  expect_s3_class(mlm_class, "classifier")
  expect_equal(initial_pred, c(0.2, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4,
                               0.4, 0.4, 0.4, 0.6, 0.6))
})
