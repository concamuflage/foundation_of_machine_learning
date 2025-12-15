# for calculating confidence interval of an estimate of proportion
source("confidence_interval.R")

#------------change area ---------------
sample_proportion = 42/125  # the percentage 
level =   0.95           # not alpha, but confidence level
number_of_samples = 125
  
# -----------do not change the following----------

intermediate_formula = sample_proportion*(1-sample_proportion) /number_of_samples
standard_error = sqrt(intermediate_formula)
alpha = 1- level

# two sided
z_critical= qnorm(1-alpha/2)
z_critical
intervalTwoSided(z_critical,standard_error,sample_proportion)

# find the lower bound such that P{population_mean>lower_bound} = level
z_critical= qnorm(1-alpha)
z_critical
intervalRightSided(z_critical,standard_error,sample_proportion) 

# find the upper bound such that P{population_mean<upper_bound} = level
z_critical= qnorm(alpha)
z_critical
intervalLeftSided(z_critical,standard_error,sample_proportion)