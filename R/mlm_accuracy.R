mlm_accuracy <- function(refitted, response){
  pred_accuracy_vec <- rep(0, length = length(refitted))
  for (j in seq_len(length(refitted))){
    pred_accuracy_vec[j] <- sqrt(sum((refitted[[j]][, response] -
                                        refitted[[j]]$Label)^2) /
                                   nrow(refitted[[j]]))
  }
  pred_accuracy_vec
}
