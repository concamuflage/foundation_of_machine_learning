
data <- c(1.1, 0.9, 1.3, 0.7, 0.8, 1.0, 1.2, 0.6, 0.9, 1.4)
mu0 <- 0.5
alpha <- 0.05


# -----------------------------
# USING t.test()
# -----------------------------
cat("=== BUILT-IN t.test() FUNCTION ===\n\n")

# Two-sided test
cat("Two-sided test\n")
result_two <- t.test(data, mu = mu0, alternative = "two.sided", conf.level = 1 - alpha)
print(result_two)
cat("\n")

# Right-sided test
cat("Right-sided test\n")
result_right <- t.test(data, mu = mu0, alternative = "greater", conf.level = 1 - alpha)
print(result_right)
cat("\n")

# Left-sided test
cat("Left-sided test\n")
result_left <- t.test(data, mu = mu0, alternative = "less", conf.level = 1 - alpha)
print(result_left)
cat("\n")

# -----------------------------
# Extract and print key results
# -----------------------------
cat("=== EXTRACTED RESULTS ===\n")
cat("Two-sided p-value:", result_two$p.value, "\n")
cat("Right-sided p-value:", result_right$p.value, "\n")
cat("Left-sided p-value:", result_left$p.value, "\n")