
# the result is a numeric vector of 100 numbers starting at -5, ending at 5, evenly spaced.
x <- seq(-5, 5, length.out = 100)

y <- 3 + 2*x - 0.5*x^2 + rnorm(100, 0, 2)

# Model 1: Simple Linear Regression
m1 <- lm(y ~ x)
# Model 2: Add Squared Term
m2 <- lm(y ~ x + I(x^2))

par(mfrow = c(1, 2))
plot(x, y, main = "Simple Linear Fit")
abline(m1, col = "red")

plot(x, y, main = "Quadratic Fit")
lines(sort(x), fitted(m2)[order(x)], col = "blue", lwd = 2)