source("confidence_interval.R")
# script for calculating interval estimators of mean with known population variance


# -------------known population variance------------------

# ----------edit section -------------
level = 0.95
population_sd = 0.3 # not variance
sample_mean = 5.6
number_of_samples = 50
# ----------edit section -------------


standard_error = population_sd/sqrt(number_of_samples)
alpha = 1- level

# two sided
z_critical= qnorm(1-alpha/2)
intervalTwoSided(z_critical,standard_error,sample_mean)

# find the lower bound such that P{population_mean>lower_bound} = level
z_critical= qnorm(1-alpha)
intervalRightSided(z_critical,standard_error,sample_mean) 

# find the upper bound such that P{population_mean<upper_bound} = level
z_critical= qnorm(alpha)
intervalLeftSided(z_critical,standard_error,sample_mean)


