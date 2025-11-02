# script for t test

# advertisement is applied in the second week. Does the advertisement work?

# plug in the numbers
first_week  <- c(46, 54, 74, 60, 63, 45) #sales in first week.
second_week <- c(54, 60, 96, 75, 80, 50) # sales in second week.
difference = first_week - second_week

# H0: mu of difference is larger than 0.(advertisement doesn't work.)
# H1:  mu of difference is smaller than 0.(advertisement works)

data_vector = difference
sample_mean = mean(data_vector)
mu0 = 0
sample_sd = sd(data_vector) # do not put variance here. it is standard deviation.
sample_size = length(data_vector)
alpha = 0.01 


# do not change the following

t_statistic = (sample_mean - mu0) /(sample_sd/sqrt(sample_size))

# two sided
cat("two sided test\n")

t_critical = qt(1-alpha/2,sample_size -1)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),sample_size -1))
p_value 

if (abs(t_statistic) > abs(t_critical) ){
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
t_critical = qt(1-alpha,sample_size -1)
t_critical 
p_value = 1 - pt(t_statistic,sample_size -1)
p_value

if (t_statistic > t_critical ){
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
t_critical = qt(alpha,sample_size -1)
t_critical 
p_value = pt(t_statistic,sample_size -1)
p_value 

if (t_statistic < t_critical ){
  cat("reject\n")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject\n")
} else {
  cat("fail to reject\n")
}


