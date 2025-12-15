source("confidence_interval.R")
# script for calculating interval estimators of mean with known population variance



# -------------known population variance------------------

# ----------edit section -------------
level = 0.95
population_sd = 0.7             # not variance
sample_mean = 1.74
number_of_samples = 44
# ----------edit section -------------


standard_error = population_sd/sqrt(number_of_samples)
alpha = 1- level

# two sided
z_critical= qnorm(1-alpha/2)
z_critical
intervalTwoSided(z_critical,standard_error,sample_mean)

# find the lower bound such that P{population_mean>lower_bound} = level
z_critical= qnorm(1-alpha)
z_critical
intervalRightSided(z_critical,standard_error,sample_mean) 

# find the upper bound such that P{population_mean<upper_bound} = level
z_critical= qnorm(alpha)
z_critical
intervalLeftSided(z_critical,standard_error,sample_mean)


