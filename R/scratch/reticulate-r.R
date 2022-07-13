reticulate::virtualenv_list()
time <- gsub(":", "", gsub(" ", "", Sys.time()))

reticulate::virtualenv_create(paste0("stressor-env", time),
                              python = Sys.which("python"))
reticulate::py_install("pycaret",
                       envname = paste0("stressor-env", time))
reticulate::use_virtualenv(paste0("stressor-env", time))
reg <- reticulate::import("pycaret.regression")
test_sine <- data_gen_sine(100000, weight_mat = matrix(rep(1, 15), nrow =3 ,
                                                       ncol = 5),
                           resp_sd = .1)
data_test <- sample(nrow(test_sine), .1 * nrow(test_sine))
test <- test_sine[data_test, ]
data <- test_sine[-data_test, ]
# Need to take it off the parallel process to run -
#  Github reference from Pycaret
exp_reg <- reg$setup(data = data, target = 'Y', n_jobs = as.integer(1))

# Does not tune the models, finely, we may want to add a function that
#  lets the user specify the creation

# TODO: Create a PyScript that returns the predictive Accuracy
models <- reg$compare_models(n_select = as.integer(50))
source_python("R/scratch/test.py")


