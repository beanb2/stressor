create_virtualenv <- function(python = "3.8.10", delete_env = FALSE) {
  if (Sys.getenv("RETICULATE_PYTHON") != "") {
    Sys.setenv("RETICULATE_PYTHON" = "")
  }
  current_virtualenv <- reticulate::virtualenv_list()
  stressor_env <- grep("stressor", current_virtualenv)
  active_env <- vector(mode = "logical", length = length(stressor_env))
  for (i in seq_len(length(current_virtualenv))) {
    check <- stressor_env[i]
    active_env[i] <- reticulate::virtualenv_exists(current_virtualenv[check])
  }
  if ((length(stressor_env) == 0 || sum(active_env) == 0)
           && delete_env == FALSE) {
    # Create the new environment
    time <- gsub(":", "", gsub(" ", "", Sys.time()))

    reticulate::virtualenv_create(paste0("stressor-env", time),
                                  python = python)
    message(paste("Created Virtual Environment:",
                  paste0("stressor-env", time)))
    message("Installing pycaret")
    reticulate::py_install("pycaret",
                           envname = paste0("stressor-env", time))
    reticulate::py_install("fugue",
                           envname = paste0("stressor-env", time))
    message("Installed pycaret")
    reticulate::use_virtualenv(paste0("stressor-env", time))
    message(paste("Using Virtual Environment:",
                  paste0("stressor-env", time)))
  } else if (length(stressor_env) > 0 && delete_env == FALSE) {
    # Use the most recent stressor env
    if (active_env[stressor_env[length(stressor_env)]]) {
      use <- stressor_env[length(stressor_env)]
      message(paste("Using Virtual Environment:",
                  current_virtualenv[use]))
      reticulate::use_virtualenv(current_virtualenv[use], required = TRUE)
    }
  } else if (length(stressor_env) > 0 && delete_env == TRUE) {
    # Remove the stressor env
    stressor_env <- grep("stressor", current_virtualenv)
    if (length(stressor_env) > 0) {
      for (i in seq_len(length(stressor_env))) {
        rem_env <- stressor_env[i]
        if (active_env[rem_env]) {
          message(paste("Removing Virtual Environmnet:",
                        current_virtualenv[rem_env]))
          reticulate::virtualenv_remove(current_virtualenv[rem_env],
                                        confirm = FALSE)
        }
      }
    }
  } else {
    stop("ERROR:: No Virtual Environments exist!")
  }
}
