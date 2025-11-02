# Testing Equality of Means of Two Normal Populations With known variances
# use Z test
# trying to prove the claim that the difference is bigger than a certain number.


# hypothesis (left_sided)

# H0: mu0 - mu1 >= difference
# H1: mu0 - mu1 < difference

# data
tires_A <- c(66.4, 61.6, 60.5, 59.1, 63.6, 61.4, 62.5, 64.4, 60.7)
tires_B <- c(58.2, 60.4, 55.2, 62.0, 57.3, 58.7, 56.1)
alpha = 0.05

# sample 1
mean_1 = mean(tires_A)
sd_1 = 3
sample_size_1 = 9
# sample 2
mean_2 = mean(tires_B)
sd_2 = 4
sample_size_2 = 7 

# calculate the statistic
z_statistic = (mean_1 - mean_2)/sqrt(sd_1^2/sample_size_1 + sd_2^2/sample_size_2)
z_statistic
# calculate the critical value
z_critical = qnorm( 1-alpha)
z_critical
# calculate the p_value
p_value = 1 - pnorm(z_statistic)
p_value

if (z_statistic > z_critical) {
  cat("reject")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject")
} else {
  cat("fail to reject")
}