set.seed(1)

# Generate linear data
n  <- 120
x  <- runif(n, 0, 10)
y  <- 3 + 2*x + rnorm(n, 0, 1)

mA <- lm(y ~ x)

png("residual_diagnostics.png", width = 1200, height = 550)
par(mfrow = c(1, 2))

plot(fitted(mA), resid(mA),
     main = "A: Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(resid(mA), main = "A: Normal Q-Q Plot")
qqline(resid(mA), col = "red")

dev.off()