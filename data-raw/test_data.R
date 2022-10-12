#Toy Test data sets
lm_test <- data_gen_lm(10)
sine_test <- data_gen_sine(10)
asym_test <- data_gen_asym(10)

usethis::use_data(lm_test, sine_test, asym_test, internal = TRUE)
