# ------------------------------------------
# Simulation: coverage probability of lower one-sided bound
# it demonstrates that 95% of the lower bounds calculated this way will be smaller than the given population mean.
# you can change your true mean to any number you want, but the success rate is around 95%. This is amazing!

# ------------------------------------------

set.seed(123)  # for reproducibility

# True parameters
true_mean <- -20 
population_sd <- 10
n <- 25
alpha <- 0.05
num_samples <- 1000000

# Precompute constants
standard_error <- population_sd / sqrt(n)
z_critical <- qnorm(1 - alpha)  # one-sided critical value (≈ 1.645)

# Initialize success counter
success_count <- 0

# Repeat sampling
for (i in 1:num_samples) {
  # Generate a random sample
  sample_data <- rnorm(n, mean = true_mean, sd = population_sd)
  
  # Compute sample mean
  sample_mean <- mean(sample_data)
  
  # Compute lower one-sided bound
  lower_bound <- sample_mean - z_critical * standard_error
  
  # Check if bound is below true mean
  if (lower_bound <= true_mean) {
    success_count <- success_count + 1
  }
}

# Compute success rate
success_rate <- success_count / num_samples

cat("Number of samples:", num_samples, "\n")
cat("Confidence level:", (1 - alpha) * 100, "%\n")
cat("Success rate (bounds below true mean):", round(success_rate * 100, 2), "%\n")