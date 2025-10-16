

set.seed(1)
n  <- 200
x  <- runif(n, 1, 10) # random uniform
# heteroscedastic y: sd grows with x
y  <- 3 + 0.8*x + rnorm(n, sd = 0.6*x)

## Fit models
m_lin  <- lm(y ~ x)
m_log  <- lm(log(y) ~ x)   # log-transform response

## Residual checks
par(mfrow = c(1, 2))
plot(fitted(m_lin), resid(m_lin),
     main = "Raw: Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals"); abline(h=0,col=2)
plot(fitted(m_log), resid(m_log),
     main = "log(y): Residuals vs Fitted", xlab = "Fitted (on log scale)", ylab = "Residuals"); abline(h=0,col=2)

## Back-transform slope interpretation (approx):
coef(m_log)
# A one-unit increase in x multiplies median(y) by exp(beta_x).