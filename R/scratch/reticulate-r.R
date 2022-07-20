reticulate::virtualenv_list()
time <- gsub(":", "", gsub(" ", "", Sys.time()))

reticulate::virtualenv_create(paste0("stressor-env", time),
                              python = Sys.which("python"))
reticulate::py_install("pycaret",
                       envname = paste0("stressor-env", time))
# Clear the RETICULATE_PYTHON env name
reticulate::use_virtualenv(paste0("stressor-env", time))
reg <- reticulate::import("pycaret.regression")
test_sine <- data_gen_sine(10000, weight_mat = matrix(rep(1, 15), nrow =3 ,
                                                       ncol = 5),
                           resp_sd = .1)
data_test <- sample(nrow(test_sine), .1 * nrow(test_sine))
test <- test_sine[data_test, ]
data <- test_sine[-data_test, ]
# Need to take it off the parallel process to run -
#  Github reference from Pycaret
exp_reg <- reg$setup(data = data, target = 'Y', n_jobs = as.integer(4))

# Does not tune the models, finely, we may want to add a function that
#  lets the user specify the creation

# TODO: Create a PyScript that returns the predictive Accuracy
# Change the number to 9999 as a "bogus number"
models <- reg$compare_models(n_select = as.integer(50), errors = "raise")
Model <- reg$pull()$Model
rmse <- vector(mode = "numeric", length = length(Model))

pred_accuracy <- data.frame(Model, rmse)
for (i in seq_len(length(models))) {
  pred_results <- reg$predict_model(models[[i]], data = test)
  pred_accuracy[i, 2] <- sqrt(sum((test$Y - pred_results$Label)^2) / nrow(test))
}
pred_accuracy
# Try this later to see which is faster:
reticulate::source_python("R/scratch/test.py")

# Put into functions
# Parallelization
# Taking real datasets - EZTune

