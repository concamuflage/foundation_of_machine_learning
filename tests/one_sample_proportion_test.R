source("compare.R")
source("confidence_interval.R")

# One sample population test

# ----------the following is from professor ----------------
# conditions:
# population size >= 10*sample size
# n is big enough so that np >= 10 (when p is small)or n(1-p)(when p is large) >= 10 

p = 0.2
p0 = 0.31
n = 100
alpha = 0.05

z_statistic = (p - p0) / sqrt(p0*(1-p0)/n)

# two sided
cat("two sided test\n")

z_critical= qnorm(1-alpha/2)
z_critical
p_value = 2*(1-pnorm(abs(z_statistic)))
p_value 
compareTwoSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)

standard_error = sqrt(p0*(1-p0)/n)
intervalTwoSided(z_critical,standard_error,p)


# right sided
cat("right sided test\n")
z_critical= qnorm(1-alpha)
z_critical
p_value = 1 - pnorm(z_statistic)
p_value
compareRightSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)



# left sided
cat("left sided test\n")
z_critical= qnorm(alpha)
z_critical
p_value = pnorm(z_statistic)
p_value 
compareLeftSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)

# -----------------------the following is based on textbook----------------

# ----------change ----------------
x = 20
number_of_samples = 100
mu0 = 0.31
alpha = 0.05
# ---------------------------------


# two _sided
cat("two sided test")

probability_one = pbinom(x,number_of_samples,mu0)
probability_two = 1 -pbinom(x,number_of_samples,mu0)
p_value = 2*min(probability_one,probability_two)
p_value
comparePvalueAlpha(p_value,alpha)

# right_sided
cat("right sided test")

p_value = 1-pbinom(x-1,number_of_samples,mu0) 
p_value
comparePvalueAlpha(p_value,alpha)

# left_sided

p_value = pbinom(x,number_of_samples,mu0)
p_value
comparePvalueAlpha(p_value,alpha)






