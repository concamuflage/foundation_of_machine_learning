# Demonstration: Linear vs Quadratic Regression

# 1. Simulate nonlinear data (a curved relationship)
set.seed(1)
x <- seq(-5, 5, length.out = 100)
y <- 2 + 1.5*x - 0.4*x^2 + rnorm(100, sd = 1.5)

# 2. Fit models
m1 <- lm(y ~ x)              # Linear model
m2 <- lm(y ~ x + I(x^2))     # Quadratic model

# 3. Compare fitted lines
par(mfrow = c(1, 2))

plot(x, y, main = "Linear Model Fit", pch = 19, col = "gray")
abline(m1, col = "red", lwd = 2)
legend("topleft", legend = "Linear fit", col = "red", lwd = 2)

plot(x, y, main = "Quadratic Model Fit", pch = 19, col = "gray")
lines(sort(x), fitted(m2)[order(x)], col = "blue", lwd = 2)
legend("topleft", legend = "Quadratic fit", col = "blue", lwd = 2)

# 4. Residual diagnostic plots
par(mfrow = c(2, 2))
plot(m1, which = 1, main = "Residuals (Linear Model)")
plot(m2, which = 1, main = "Residuals (Quadratic Model)")
plot(m1, which = 2, main = "QQ Plot (Linear Model)")
plot(m2, which = 2, main = "QQ Plot (Quadratic Model)")