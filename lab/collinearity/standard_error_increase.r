## Collinearity inflates SEs: simulation --------------------------------------
set.seed(123)

# For a given correlation rho = cor(x1, x2), simulate data and average SEs
simulate_SEs <- function(rho, n = 200, reps = 200, beta = c(1, 1), sigma = 1) {
  se1 <- se2 <- numeric(reps)
  for (r in 1:reps) {
    x1 <- rnorm(n)
    z  <- rnorm(n)
    x2 <- rho * x1 + sqrt(1 - rho^2) * z       # make cor(x1, x2) ≈ rho
    y  <- beta[1] * x1 + beta[2] * x2 + rnorm(n, sd = sigma)

    fit <- lm(y ~ x1 + x2)
    se  <- sqrt(diag(vcov(fit)))[-1]           # SEs of coefficients (x1, x2)
    se1[r] <- se[1];  se2[r] <- se[2]
  }
  c(rho = rho, SE_x1 = mean(se1), SE_x2 = mean(se2))
}

rhos <- seq(0, 0.98, by = 0.14)                # from no collinearity to near-perfect
res  <- as.data.frame(t(sapply(rhos, simulate_SEs)))
print(round(res, 3))

# Plot: SE vs correlation between predictors
ylim <- range(res$SE_x1, res$SE_x2)
plot(res$rho, res$SE_x1, type = "b", pch = 19, ylim = ylim,
     xlab = expression(rho == cor(x[1], x[2])),
     ylab = "Average Std. Error", main = "Collinearity inflates coefficient SEs")
lines(res$rho, res$SE_x2, type = "b", pch = 1)
legend("topleft", c("SE of x1", "SE of x2"), lty = 1, pch = c(19, 1))

# (Optional) theoretical VIF growth for two-predictor case: VIF = 1/(1 - rho^2)
vif_th <- 1 / (1 - rhos^2)
plot(rhos, vif_th, type = "b", pch = 19,
     xlab = expression(rho), ylab = "Theoretical VIF",
     main = "Variance Inflation with Collinearity")