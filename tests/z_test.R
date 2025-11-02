# script for z test

# conditions:
# one sample
# when population standard deviation is known



# plug in the numbers

sample_mean = 1
mu0 = 0.5
population_sd = 0.5 # do not put variance here. it is standard deviation.
sample_size = 10
alpha = 0.05 

# do not change the following

z_statistic = (sample_mean - mu0) /(population_sd/sqrt(sample_size))

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


