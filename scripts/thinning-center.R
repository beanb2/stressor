# Data Thinning Visualization

data <-data_gen_lm(10000, resp_sd = 1)
asym_model <- lm(Y ~., data = data)
pred_thin <- thinning(asym_model, data)
