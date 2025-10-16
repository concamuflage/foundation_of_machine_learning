

# Given values
xbar <- 3.2      # sample mean
s <- 4.5         # sample standard deviation
n <- 18             # number of samples
mu0 <- 0          # null hypothesis mean

# t statistic
t_stat <- (xbar - mu0) / (s / sqrt(n))
t_stat

qt(0.95,17)

sharks <- c(18.1, 23.4, 23.8, 24.1, 22.5, 19, 25.4, 23.1, 16.5, 26.7)
t.test(sharks,mu = 20, conf.level = 0.9)

1-pt(2.16,9)
