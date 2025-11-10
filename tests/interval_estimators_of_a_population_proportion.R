# the true proportion is unknown

source("confidence_interval.R")
# script for calculating interval estimators of differences in proportions of compulation.

# ----------edit section -------------
level = 0.95
sample_proportion_one = 0.5
sample_proportion_two = 0.6
sample_size_one = 200
sample_size_two = 100


# ----------edit section -------------

standard_error = sqrt(sample_proportion_one*(1-sample_proportion_one)/sample_size_one+sample_proportion_two*(1-sample_proportion_two)/sample_size_two)
alpha = 1- level
difference = sample_proportion_one - sample_proportion_two

# two sided
z_critical= qnorm(1-alpha/2)
intervalTwoSided(z_critical,standard_error,difference)

# find the lower bound such that P{population_mean>lower_bound} = level
z_critical= qnorm(1-alpha)
intervalRightSided(z_critical,standard_error,difference) 

# find the upper bound such that P{population_mean<upper_bound} = level
z_critical= qnorm(alpha)
intervalLeftSided(z_critical,standard_error,difference)
