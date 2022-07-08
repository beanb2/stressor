install_pycaret <- function(method = "auto", conda = "auto") {
  reticulate::py_install("pycaret", method = method, conda = conda)
}

# What happens when you throw in Noise Variables
# Work on getting the reticulate package to run it in R
