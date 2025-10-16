# the following code
# shows that drawing a sample of size 10 from a normal distribution
# the resulting t_statistic follows a t distribution of df 9. 


set.seed(123)

# Population parameters
mu <- 50
sigma <- 10

# Sample size
n <- 10   # try changing to 30 or 100 to see convergence to normal

# Number of simulations
N <- 100000

# Simulate T statistics
t_values <- replicate(N, {
  x <- rnorm(n, mean = mu, sd = sigma)      # sample from normal
  xbar <- mean(x)
  s <- sd(x)
  t_stat <- (xbar - mu) / (s / sqrt(n))     # Student's t-statistic
  return(t_stat)
})

# Plot histogram of simulated T values
hist(t_values, probability = TRUE, breaks = 50,
     main = paste("Empirical T distribution for n =", n),
     xlab = "t", col = "lightblue", border = "white")

# Overlay theoretical t density
curve(dt(x, df = n - 1), add = TRUE, col = "red", lwd = 2)

# Overlay normal density for comparison
curve(dnorm(x), add = TRUE, col = "darkgreen", lwd = 2, lty = 2)

legend("topright",
       legend = c("Simulated T", "Theoretical t(df=n-1)", "Normal N(0,1)"),
       col = c("lightblue", "red", "darkgreen"),
       lty = c(NA,1,2),
       pch = c(15, NA, NA))