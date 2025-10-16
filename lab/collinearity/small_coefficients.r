set.seed(123)

n <- 200
B <- 1000  # number of simulations

coef_x1 <- numeric(B)
coef_x2 <- numeric(B)

for (i in 1:B) {
  x1 <- rnorm(n, 0, 1)
  x2 <- 0.99 * x1 + rnorm(n, 0, 0.01)   # almost perfectly correlated with x1
  y <- 2 * x1 + 2 * x2 + rnorm(n, 0, 1)
  model <- lm(y ~ x1 + x2)
  coefs <- coef(model)[-1]  # exclude intercept
  coef_x1[i] <- coefs["x1"]
  coef_x2[i] <- coefs["x2"]
}

# Plot histograms with mean lines
par(mfrow = c(1, 2))

hist(coef_x1, breaks = 30, col = "lightblue",
     main = "Distribution of x1 Coefficient",
     xlab = "x1 Coefficient")
abline(v = mean(coef_x1), col = "red", lwd = 2)
text(mean(coef_x1), par("usr")[4]*0.9, 
     labels = paste("Mean =", round(mean(coef_x1), 2)), col = "red", pos = 4)

hist(coef_x2, breaks = 30, col = "lightgreen",
     main = "Distribution of x2 Coefficient",
     xlab = "x2 Coefficient")
abline(v = mean(coef_x2), col = "red", lwd = 2)
text(mean(coef_x2), par("usr")[4]*0.9, 
     labels = paste("Mean =", round(mean(coef_x2), 2)), col = "red", pos = 4)

cat("Average coefficient for x1:", mean(coef_x1), "\n")
cat("Average coefficient for x2:", mean(coef_x2), "\n")