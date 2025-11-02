# conditions:
# Suppose the variances of the two population is unknown 
# sample size >= 30



# hypothesis (right_handed)

# H0: mu0 <= mu1
# H1: mu0 > mu1

# data
tires_A <- c(66.4, 61.6, 60.5, 59.1, 63.6, 61.4, 62.5, 64.4, 60.7)
tires_B <- c(58.2, 60.4, 55.2, 62.0, 57.3, 58.7, 56.1)
alpha = 0.05 

# sample 1
mean_1 = 242
sd_1 = sqrt(62.2) # do not put variance here by mistake
sample_size_1 = 30

# sample 2
mean_2 = 234
sd_2 = sqrt(58.4) # do not put variance here by mistake
sample_size_2 = 20 

# calculate the statistic
z_statistic = (mean_1 - mean_2)/sqrt(sd_1^2/sample_size_1 + sd_2^2/sample_size_2)
z_statistic
# calculate the critical value
z_critical = qnorm(1-alpha)
z_critical
# calculate the p_value
p_value =1 - pnorm(z_statistic)
p_value

if (z_statistic > z_critical){
  cat("reject")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject")
} else {
  cat("fail to reject")
}