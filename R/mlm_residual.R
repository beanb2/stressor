mlm_residual <- function(predictions, observed){
  resid <- matrix(0, nrow = nrow(predictions), ncol = ncol(predictions))
  for (i in seq_len(ncol(predictions))) {
    resid[, i] <- observed - predictions[, i]
  }
  colnames(resid) <- colnames(predictions)
  resid <- as.data.frame(resid)
  resid
}
