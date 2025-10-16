
set.seed(1)
x <- 1:100
y <- 3*x + runif(100, 0, 20)
model <- lm(y ~ x)

# fitted(model) is the y.
plot(fitted(model), resid(model), 
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted(the dependable variable)")
abline(h = 0, col = "red")