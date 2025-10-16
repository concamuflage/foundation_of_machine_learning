set.seed(2)
n  <- 200
x  <- runif(n, 0, 8)
mu <- 1 + 0.9*x                  # mean increases with x
y  <- rpois(n, lambda = pmax(mu, 0.1))  # counts; variance ≈ mean -> heteroscedastic

## Fit models
m_lin  <- lm(y ~ x)
m_sqrt <- lm(sqrt(y) ~ x)        # variance-stabilizing for Poisson-like data

## Residual checks
par(mfrow = c(1, 2))
plot(fitted(m_lin), resid(m_lin),
     main = "Raw counts: Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals"); abline(h=0,col=2)
plot(fitted(m_sqrt), resid(m_sqrt),
     main = "sqrt(y): Residuals vs Fitted", xlab = "Fitted (sqrt scale)", ylab = "Residuals"); abline(h=0,col=2)

## Optional: compare fits numerically
c(R2_raw = summary(m_lin)$adj.r.squared,
  R2_sqrt = summary(m_sqrt)$adj.r.squared)