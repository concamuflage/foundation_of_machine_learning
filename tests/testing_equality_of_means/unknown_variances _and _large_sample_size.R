# conditions:
# population distribution can be non-normal because if the sample size is large, its mean estimator is still approximately normal.
# Suppose the variances of the two population is unknown 
# sample size >= 30



# hypothesis (two sided)

# H0: mu0 = mu1
# H1: mu0 != mu1

# data
tires_A <- c(66.4, 61.6, 60.5, 59.1, 63.6, 61.4, 62.5, 64.4, 60.7)
tires_B <- c(58.2, 60.4, 55.2, 62.0, 57.3, 58.7, 56.1)
alpha = 0.05 

# sample 1
mean_1 = 8.2
sd_1 = 5.4 # do not put variance here by mistake
sample_size_1 = 50
# sample 2
mean_2 = 8.8
sd_2 = 4.5 # do not put variance here by mistake
sample_size_2 = 50 

# calculate the statistic
z_statistic = (mean_1 - mean_2)/sqrt(sd_1^2/sample_size_1 + sd_2^2/sample_size_2)
z_statistic

# two sided
cat("two sided test\n")

z_critical= qnorm(1-alpha/2)
z_critical
p_value = 2*(1-pnorm(abs(z_statistic)))
p_value 

if (abs(z_statistic) > abs(z_critical) ){
  cat("reject\n")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject\n")
} else {
  cat("fail to reject\n")
}

# right sided
cat("right sided test\n")
z_critical= qnorm(1-alpha)
z_critical
p_value = 1 - pnorm(z_statistic)
p_value

if (z_statistic > z_critical){
  cat("reject\n")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject\n")
} else {
  cat("fail to reject\n")
}


# left sided
cat("left sided test\n")
z_critical= qnorm(alpha)
z_critical
p_value = pnorm(z_statistic)
p_value 

if (z_statistic < z_critical){
  cat("reject\n")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject\n")
} else {
  cat("fail to reject\n")
}
