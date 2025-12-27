source("compare.R")

# script for z test

# conditions:
# one sample
# when population standard deviation is known



# plug in the numbers

sample_mean = 103.29
mu0 = 100
population_sd = 5 #  NOT variance
sample_size = 43
alpha = 0.05

# do not change the following

z_statistic = (sample_mean - mu0) /(population_sd/sqrt(sample_size))
z_statistic
# two sided
cat("two sided test\n")

z_critical= qnorm(1-alpha/2)
z_critical
p_value = 2*(1-pnorm(abs(z_statistic)))
p_value 
compareTwoSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)


# right sided( H1: mu > mu0) NOT H0
cat("right sided test\n")
z_critical= qnorm(1-alpha)
z_critical
p_value = 1 - pnorm(z_statistic)
p_value
compareRightSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)



# left sided ( H1: mu < mu0) NOT H0
cat("left sided test\n")
z_critical= qnorm(alpha)
z_critical
p_value = pnorm(z_statistic)
p_value 
compareLeftSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)




