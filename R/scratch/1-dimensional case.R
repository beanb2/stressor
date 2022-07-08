# Go through the 1 dimensional case, try to figure out what is happening, data
#  generation issue, coefficient predicting
set.seed(43421)
one_d <- data_gen_lm(100, 1, y_int = 0, resp_sd = .1)
simple_lm <- lm(Y ~ ., one_d)
test_d <- data_gen_lm(300, 1, y_int = 0, resp_sd = .1)
pred <- predict(simple_lm, test_d)

accuracy <- sum(test_d$Y - pred) ** 2

# Setting up a stable and portable environment/Anaconda
