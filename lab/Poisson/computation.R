# Compare computation time: Binomial vs Poisson

# trying to demonstrate that Binomial distribution is more 
# expensive to calculate than Poisson distribution 
# but the lab result is contrarary.

# ---------------------------------------------

n <- 10000
p <- 0.0003
k <- 0:1000000
lambda <- n * p

# Time the Binomial computation
t_binom <- system.time({
  binom_probs <- dbinom(k, n, p)
})

# Time the Poisson computation
t_pois <- system.time({
  pois_probs  <- dpois(k, lambda)
})

# Show results and times
result <- cbind(k, binom_probs, pois_probs)
print(result)

cat("\nComputation time (seconds):\n")
cat(sprintf("Binomial: %.6f\n", t_binom["elapsed"]))
cat(sprintf("Poisson:  %.6f\n", t_pois["elapsed"]))