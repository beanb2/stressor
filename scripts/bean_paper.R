# Validation of Dr. Beans Code

# Need for my code
Sys.unsetenv("RETICULATE_PYTHON")
library(stressor)
library(thermor)

# Initiate Virtual-env
create_virtualenv()

# Load Data
data("thermo_orig")
formula <- kLa ~ PIVWm3 + vvmDHS + vvmFRIT + slpmfrit + Volume + ImpDiacm +
  colheight + Agitation + poreSize + fluxArea + GEV + pore

# Create Machine Learning Models
mlm_thermo <- mlm_regressor(formula, thermo_orig)

# Run a 10-fold cross validation with replicates
cv_results <- cv(mlm_thermo, thermo_orig, n_folds = 10, repl = TRUE)
cv_rmse <- rmse(cv_results, thermo_orig$kLa)
cv_rmse

# 50 time study
rmse_mat <- matrix(0, nrow = 18, ncol = 50)
for (i in seq_len(50)){
  cv_results <- cv(mlm_thermo, thermo_orig, n_folds = 10, repl = TRUE)
  cv_rmse <- rmse(cv_results, thermo_orig$kLa)
  rmse_mat[, i] <- cv_rmse$rmse
}
rownames(rmse_mat) <- mlm_thermo$pred_accuracy$Model
average <- apply(rmse_mat, 1, mean)
