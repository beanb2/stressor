create_virtualenv <- function(python = "3.8.10", delete_env = FALSE) {
  current_virtualenv <- reticulate::virtualenv_list()
  if (length(current_virtualenv) > 0 && delete_env == FALSE) {
    #TODO: Use the most recent stressor env
  } else if (length(current_virtualenv) > 0 && delete_env == TRUE) {
    #TODO: Remove the stressor env
  } else if (length(current_virtualenv)) {

  }
}
