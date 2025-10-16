set.seed(42)

# True population
population <- rnorm(1000000, mean = 50, sd = 10)
true_variance <- var(population) # This uses n-1, close to true σ² = 100


# Parameters
sample_size <- 10
num_samples <- 10000

# Collect sample variances
var_n_minus1 <- numeric(num_samples)
var_n <- numeric(num_samples)

for (i in 1:num_samples) {
    sample_data <- sample(population, sample_size)

    # 1) Corrected (n-1) variance (default in var())
    var_n_minus1[i] <- var(sample_data)

    # 2) Uncorrected (divide by n)
    var_n[i] <- sum((sample_data - mean(sample_data))^2) / sample_size
}

# Compare the averages
avarage_n_minus_1 = mean(var_n_minus1) # unbiased estimate ≈ true variance
average_n = mean(var_n) # biased estimate (smaller)

png("bessels_variance_hist.png")

hist(var_n,
     breaks = 50, col = rgb(1, 0, 0, 0.4), xlim = c(60, 120),
     main = "Sample Variance Estimates",
     xlab = "Variance estimate", border = NA)
hist(var_n_minus1, breaks = 50, col = rgb(0, 0, 1, 0.4), add = TRUE, border = NA)
abline(v = true_variance, col = "yellow", lwd = 3)
abline(v = avarage_n_minus_1, col = "blue", lwd = 3)
abline(v = average_n, col = "red", lwd = 3)
legend("topright",
       legend = c("Divide by n", "Divide by n-1", "True variance"),
       col = c("red", "blue", "darkgreen"), lwd = 3)

dev.off()
