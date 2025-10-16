set.seed(1)

# this will produce a residual plot with a curvature in it.

# --- Generate data and fit models ---
x <- 1:1000000

y1 <- 3*x + 0.5*x^2 + rnorm(1000000, 0, 20)
m1 <- lm(y1 ~ x)



# --- Save to a single PNG file ---
png("residuals_comparison.png", width = 1500, height = 500)
par(mfrow = c(1, 3))  # 1 row, 3 columns layout

# Plot 1: Normal noise
plot(fitted(m1), resid(m1),
     main = "Normal Noise",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "blue", pch = 19)
abline(h = 0, col = "red", lwd = 2)


dev.off()

cat("Residual plots saved to 'residuals_comparison.png'\n")