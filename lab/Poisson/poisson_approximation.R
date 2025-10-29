# Poisson Approximation to Binomial Distribution

# if the number of trial is large and p is small, 
# then the number of success is a Poisson random variable
# with lamda = np.


# ---------------------------------------------

# Parameters
n <- 100      # number of trials
p <- 0.03      # probability of success
lambda <- n * p  # Poisson mean

# Range of x values to plot
x <- 0:n

# Compute probabilities
binom_probs <- dbinom(x, size = n, prob = p)
pois_probs  <- dpois(x, lambda = lambda)

# Combine into a data frame
df <- data.frame(
  x = x,
  Binomial = binom_probs,
  Poisson = pois_probs
)

# --- Plot in RStudio ---
# Plot Binomial first
plot(x, binom_probs, type = "h", lwd = 3, col = "blue",
     main = "Poisson Approximation to Binomial Distribution",
     xlab = "x", ylab = "Probability",
     ylim = c(0, max(binom_probs, pois_probs)))
# Add Poisson on top
points(x, pois_probs, type = "b", col = "red", pch = 19)

# Add legend
legend("topright", legend = c("Binomial(n=100, p=0.03)", "Poisson(λ=3)"),
       col = c("blue", "red"), lwd = 2, pch = c(NA, 19))
