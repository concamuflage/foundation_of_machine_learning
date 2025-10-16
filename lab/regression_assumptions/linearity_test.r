set.seed(1)

# --- Generate data and fit models ---
x <- 1:100

# 1. Normal noise
y1 <- 3*x + rnorm(100, 0, 20)
m1 <- lm(y1 ~ x)

# 2. Uniform noise
y2 <- 3*x + runif(100, 0, 20)
m2 <- lm(y2 ~ x)

# 3. Poisson noise
y3 <- 3*x + rpois(100, lambda = 20)
m3 <- lm(y3 ~ x)

# --- Save to a single PNG file ---
png("residuals_comparison.png", width = 1500, height = 500)
par(mfrow = c(1, 3))  # 1 row, 3 columns layout

# Plot 1: Normal noise
plot(fitted(m1), resid(m1),
     main = "Normal Noise",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "blue", pch = 19)
abline(h = 0, col = "red", lwd = 2)

# Plot 2: Uniform noise
plot(fitted(m2), resid(m2),
     main = "Uniform Noise",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "darkorange", pch = 17)
abline(h = 0, col = "red", lwd = 2)

# Plot 3: Poisson noise
plot(fitted(m3), resid(m3),
     main = "Poisson Noise",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "darkgreen", pch = 15)
abline(h = 0, col = "red", lwd = 2)

dev.off()

cat("Residual plots saved to 'residuals_comparison.png'\n")