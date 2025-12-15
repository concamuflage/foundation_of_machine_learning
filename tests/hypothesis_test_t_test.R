source("compare.R")

# script for t test

# ----change area ------
#first_week  <- c(46, 54, 74, 60, 63, 45)
#second_week <- c(54, 60, 96, 75, 80, 50)
#difference = first_week - second_week
  
#data_vector = difference
sample_mean = 47.2
mu0 = 50
sample_sd = sqrt(3.1) # NOT VARIANCE
sample_size = 8
alpha = 0.05 
# -------change area ---------

# do not change the following

t_statistic = (sample_mean - mu0) /(sample_sd/sqrt(sample_size))
t_statistic
df = sample_size -1

# two sided
cat("two sided test\n")

t_critical = qt(1-alpha/2,df)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),df))
p_value 

compareTwoSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)

# right sided ( H1: mu > mu0) NOT H0
cat("right sided test\n")
t_critical = qt(1-alpha,df)
t_critical 
p_value = 1 - pt(t_statistic,df)
p_value

compareRightSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)


# left sided ( H1: mu < mu0) NOT H0
cat("left sided test\n")
t_critical = qt(alpha,df)
t_critical 
p_value = pt(t_statistic,df)
p_value 

compareLeftSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)


