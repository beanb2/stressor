reticulate::virtualenv_list()
time <- gsub(":", "", gsub(" ", "", Sys.time()))

reticulate::virtualenv_create(paste0("stressor-env", time),
                              python = Sys.which("python"))
reticulate::py_install("pycaret",
                       envname = paste0("stressor-env", time))
reticulate::use_virtualenv(paste0("stressor-env", time))
reg <- reticulate::import("pycaret.regression")
data_test <- sample(nrow(test_sine), .1 * nrow(test_sine))
test_y <- test_sine[data_test, 1]
test <- test_sine[data_test, -1]
exp_reg <- reg$setup(data = test_sine, target = 'Y')
models <- reg$compare_models(n_select = 5)
