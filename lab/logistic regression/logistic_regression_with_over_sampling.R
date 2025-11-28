# this lab shows shows how oversampling distorts the intercept in a logistic regression.
# this is performed on a simple linear regression(with only 1 predictor x)

# --------------without oversample and fitting the model---------------------------
set.seed(123)

N <- 100000   # large population
x <- rnorm(N) # a vector of values of x.
beta0 <- -4.5  # true intercept
beta1 <- 2 # true slope

# p is a vector of probabilities, which are the true probabilities.
p <- 1 / (1 + exp(-(beta0 + beta1 * x)))

# simulate outcome

# y is a vector of 1 and 0, of length N.
# we do N Bernoulli trials, and each trial has a success rate that is from the vector of p
# so each trial has different success rate p_i
y <- rbinom(N, 1, p)

# need to know how to calculate the mean of a vector. 

# shows the percentage of elements = 1 in the vector of 1.
# need to understand the mean of a vector to see why. 

mean(y) 

# the resulting model have intercept and slope close to the true ones.
model_true <- glm(y ~ x, family = binomial)
summary(model_true)

#------------over sample ---------------------

# y is a vector, return indices in the vector where the element is 1
cases <- which(y == 1) # a vector of indices
size = length(cases)*5 # size is 5 times of number of cases.
non_cases = which(y == 0) # a vector of indices
controls <- sample(non_cases, size = size)  # sample from non_cases vector.

idx <- c(cases, controls)
x_samp <- x[idx]
y_samp <- y[idx]

mean(y_samp)   # now the proprotion of cases in the sample will be around 1/6 = 0.16, or ~16% 

# ----------fit a model over the oversampled  sample --------

# the slope is close to the true slope
# the intercept is not 
model_cc <- glm(y_samp ~ x_samp, family = binomial)
summary(model_cc)

# test on a typical x value
x0 <- 0
predict(model_true, data.frame(x = x0), type = "response")         # close to true probabilty
predict(model_cc,   data.frame(x_samp = x0), type = "response")    # much higher 

# ---------test if intercept correction formula for case–control sampling in logistic regression works -----
p_rare = 0.04738     # true proprotion
p_set = 0.16         # proprotion with oversampling
intercept_restored = -3.13922 + log(p_rare/(1-p_rare)) - log(p_set/(1-p_set))
intercept_restored     # intercept_restored is now close the real one.


