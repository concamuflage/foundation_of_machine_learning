source("compare.R")

# script for t test

# plug in the numbers
first_week  <- c(46, 54, 74, 60, 63, 45)
second_week <- c(54, 60, 96, 75, 80, 50)
difference = first_week - second_week
  
data_vector = difference
sample_mean = mean(data_vector)
mu0 = 0
sample_sd = sd(data_vector) # do not put variance here. it is standard deviation.
sample_size = length(data_vector)
alpha = 0.05 

# do not change the following

t_statistic = (sample_mean - mu0) /(sample_sd/sqrt(sample_size))

# two sided
cat("two sided test\n")

t_critical = qt(1-alpha/2,sample_size -1)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),sample_size -1))
p_value 

compareTwoSided(t_statistic,t_critical,p_value,alpha)

# right sided
cat("right sided test\n")
t_critical = qt(1-alpha,sample_size -1)
t_critical 
p_value = 1 - pt(t_statistic,sample_size -1)
p_value

compareRightSided(t_statistic,t_critical,p_value,alpha)


# left sided
cat("left sided test\n")
t_critical = qt(alpha,sample_size -1)
t_critical 
p_value = pt(t_statistic,sample_size -1)
p_value 

compareLeftSided(t_statistic,t_critical,p_value,alpha)


