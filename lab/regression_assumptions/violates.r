set.seed(2)

n  <- 120
x  <- runif(n, 0, 10)

# Nonlinear mean + heteroscedasticity + heavier tails
err <- rt(n, df = 3) * (0.3 + 0.25*x)  # variance grows with x and t-errors
y   <- 1 + 0.5*x + 0.3*x^2 + err       # true relation is curved

mB <- lm(y ~ x)  # wrongly fit a straight line

png("violates_plots.png", width = 1200, height = 550)
par(mfrow = c(1, 3))
# 1) Residuals vs Fitted: curved pattern + fan shape (increasing spread)
hist(resid(mB))
plot(fitted(mB), resid(mB),
     main = "B: Residuals vs Fitted",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

# 2) Normal Q-Q: heavy tails deviate from the line
qqnorm(resid(mB), main = "B: Normal Q-Q Plot")
qqline(resid(mB), col = "red")
dev.off()
cat("Plot saved to violates_plots.png\n")

summary(mB)$r.squared