# the true proportion is unknown

source("confidence_interval.R")
# script for calculating interval estimators of mean with known population variance

# ----------edit section -------------
level = 0.95
sample_proportion = 0.336
number_of_samples = 125
# ----------edit section -------------

standard_error = sqrt(sample_proportion*(1-sample_proportion)/number_of_samples)
alpha = 1- level

# two sided
z_critical= qnorm(1-alpha/2)
intervalTwoSided(z_critical,standard_error,sample_proportion)

# find the lower bound such that P{population_mean>lower_bound} = level
z_critical= qnorm(1-alpha)
intervalRightSided(z_critical,standard_error,sample_proportion) 

# find the upper bound such that P{population_mean<upper_bound} = level
z_critical= qnorm(alpha)
intervalLeftSided(z_critical,standard_error,sample_proportion)
