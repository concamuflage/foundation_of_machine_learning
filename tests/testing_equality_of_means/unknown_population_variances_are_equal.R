# both population variances are unknown but are supposed to be equal.



# data
vitaminC <- c(5.5, 6.0, 7.0, 6.0, 7.5, 6.0, 7.5, 5.5, 7.0, 6.5)
placebo  <- c(6.5, 6.0, 8.5, 7.0, 6.5, 8.0, 7.5, 6.5, 7.5, 6.0, 8.5, 7.0)
alpha = 0.05 

# sample 1, this correpsonds to the mu on the left of the operator,no matter one sided or two sided.
mean_1 = mean(placebo)
sd_1 = sd(placebo) # do not put variance here by mistake
sample_size_1 = length(placebo)
# sample 2, this corresponds to the mu on the right of the operator, no matter one sided or two sided.
mean_2 = mean(vitaminC)
sd_2 = sd(vitaminC) # do not put variance here by mistake
sample_size_2 = length(vitaminC)

# pool the sample variance

weighted_variance_1 = ((sample_size_1 - 1)/(sample_size_1+sample_size_2 -2))*(sd_1^2)
weighted_variance_2 = ((sample_size_2 - 1)/(sample_size_1+sample_size_2 -2))*(sd_2^2)
pooled_variance = weighted_variance_1+weighted_variance_2

# calculate the statistic
t_statistic = (mean_1 - mean_2)/sqrt(pooled_variance*(1/sample_size_1+1/sample_size_2))
t_statistic

# hypothesis (two sided)

cat("two sided")

# H0: mu0 = mu1
# H1: mu0 != mu1

# calculate the critical value
t_critical = qt(1-alpha/2,sample_size_1+sample_size_2-2)
t_critical
# calculate the p_value
p_value =2* (1 - pt(t_statistic,sample_size_1+sample_size_2-2))
p_value

if (abs(t_statistic) > abs(t_critical) ){
  cat("reject")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject")
} else {
  cat("fail to reject")
}


# hypothesis (right sided)
cat("right_sided")

# calculate the critical value
t_critical = qt(1-alpha,sample_size_1+sample_size_2-2)
t_critical
# calculate the p_value
p_value = 1 - pt(t_statistic,sample_size_1+sample_size_2-2)
p_value

if (t_statistic > t_critical ){
  cat("reject")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject")
} else {
  cat("fail to reject")
}

# hypothesis (left sided)

cat("left_sided")

# calculate the critical value
t_critical = qt(alpha,sample_size_1+sample_size_2-2)
t_critical
# calculate the p_value
p_value = pt(t_statistic,sample_size_1+sample_size_2-2)
p_value

if (t_statistic < t_critical ){
  cat("reject")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject")
} else {
  cat("fail to reject")
}
