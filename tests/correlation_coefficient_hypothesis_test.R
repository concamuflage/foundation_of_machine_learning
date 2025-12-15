source("compare.R")


# p is the correlation coefficient for the population.
# h0: p = 0
# h1: p != 0

x <- c(12,16,13,18,19,12,18,19,12,14) # change
y <- c(73,67,74,63,73,84,60,62,76,71) # change

# manual test

r = cor(x,y)  # change
alpha = 0.05 # change
sample_size = length(x) 


t_statistic = r*sqrt((sample_size -2)/(1-r^2))
t_statistic
df = sample_size -2
df

# two sided
cat("two sided test\n")

t_critical = qt(1-alpha/2,df)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),df))
p_value 
compareTwoSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)
# auto test


cor.test(x, y)