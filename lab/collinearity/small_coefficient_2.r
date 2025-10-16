set.seed(1)

# Settings: crank up collinearity and keep n modest
n    <- 40
B    <- 2000
rho  <- 0.9995        # correlation between x1 and x2 (very high)
btrue <- c(2, 2)      # true coefficients
sigma <- 1

coef_x1 <- numeric(B)
coef_x2 <- numeric(B)

for (i in 1:B) {
  x1 <- rnorm(n)
  z  <- rnorm(n)
  x2 <- rho * x1 + sqrt(1 - rho^2) * z   # cor(x1, x2) ~ rho
  y  <- btrue[1] * x1 + btrue[2] * x2 + rnorm(n, sd = sigma)

  fit <- lm(y ~ x1 + x2)
  b   <- coef(fit)[c("x1", "x2")]
  coef_x1[i] <- b[1]
  coef_x2[i] <- b[2]
}

# How often does one coefficient get ~"near zero"?
thr <- 0.2  # threshold for "near zero" (adjust to taste)
near_zero_runs <- mean( (abs(coef_x1) < thr) | (abs(coef_x2) < thr) )
cat(sprintf("Proportion of runs with at least one coeff |β̂| < %.2f: %.3f\n",
            thr, near_zero_runs))

# Summaries
summ <- function(v) round(quantile(v, c(.01,.05,.25,.5,.75,.95,.99)), 3)
cat("Quantiles for β̂_x1:\n"); print(summ(coef_x1))
cat("Quantiles for β̂_x2:\n"); print(summ(coef_x2))
cat(sprintf("Means:  mean(β̂_x1)=%.3f, mean(β̂_x2)=%.3f\n",
            mean(coef_x1), mean(coef_x2)))

# Plots
op <- par(no.readonly = TRUE); on.exit(par(op))
par(mfrow = c(1, 3))

hist(coef_x1, breaks = 40, main = expression("Histogram of " * hat(beta)[x1]),
     xlab = expression(hat(beta)[x1]))
abline(v = mean(coef_x1), col = "red", lwd = 2)
abline(v = btrue[1], col = "darkgreen", lwd = 2, lty = 2) # true

hist(coef_x2, breaks = 40, main = expression("Histogram of " * hat(beta)[x2]),
     xlab = expression(hat(beta)[x2]))
abline(v = mean(coef_x2), col = "red", lwd = 2)
abline(v = btrue[2], col = "darkgreen", lwd = 2, lty = 2) # true

plot(coef_x1, coef_x2, pch = 19, cex = 0.6,
     xlab = expression(hat(beta)[x1]), ylab = expression(hat(beta)[x2]),
     main = expression("Scatter of (" * hat(beta)[x1] * "," * hat(beta)[x2] * ")"))
abline(a = 4, b = -1, col = "blue", lwd = 2, lty = 2)  # β̂_x1 + β̂_x2 ≈ 4 ridge