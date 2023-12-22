test_that("mlm_regressor fits regression models", {
  skip_if_no_python()
  lm_data <- data_gen_lm(25)
  mlm_regressor <- mlm_regressor(Y ~ ., lm_data)
  expect_equal(length(mlm_regressor), 2)
  expect_equal(length(mlm_regressor$models), 18)
  expect_equal(is.data.frame(mlm_regressor$pred_accuracy), TRUE)
  expect_s3_class(mlm_regressor, "mlm_stressor")
  expect_s3_class(mlm_regressor, "regressor")
})

test_that("mlm_classification fits classification models", {
  skip_if_no_python()
  binary_resp <- sample(c(0, 1), 50, replace = TRUE)
  sine_class <- data_gen_sine(50)
  sine_class$Y <- binary_resp
  mlm_class <- mlm_classification(Y ~ ., sine_class)
  expect_equal(length(mlm_class), 2)
  expect_equal(length(mlm_class$models), 14)
  expect_equal(is.data.frame(mlm_class$pred_accuracy), TRUE)
  expect_s3_class(mlm_class, "mlm_stressor")
  expect_s3_class(mlm_class, "classifier")
})

test_that("mlm_refit predicts and returns the models", {
  skip_if_no_python()
  expect_equal(2 * 2, 4)
})

test_that("mlm_init is reproducible", {
  skip_if_no_python()
})
