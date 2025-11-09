source("confidence_interval.R")
# script for calculating interval estimators of mean with unknown population variance


# -------------unknown population variance------------------

# ----------edit section -------------
level = 0.99
sample_sd = 5.085 # not variance
sample_mean = 5.8
number_of_samples = 20
# ----------edit section -------------

standard_error = sample_sd/sqrt(number_of_samples)
alpha = 1- level

# two sided
t_critical= qt(1-alpha/2,number_of_samples -1)
intervalTwoSided(t_critical,standard_error,sample_mean)

# find the lower bound such that P{population_mean>lower_bound} = level
t_critical= qt(1-alpha,number_of_samples-1)
intervalRightSided(t_critical,standard_error,sample_mean) 

# find the upper bound such that P{population_mean<upper_bound} = level
t_critical= qt(alpha,number_of_samples-1)
intervalLeftSided(t_critical,standard_error,sample_mean)