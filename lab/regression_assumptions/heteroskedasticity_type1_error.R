set.seed(42)


# Simulation1
# We generate x \sim N(0,1), y \sim N(0,1) independently, so the true slope is 0. Any “significant” result is a Type I error.
#•	type1_rate ≈ 0.05, matching α = 0.05.
#•	The histogram of p-values is ~uniform on [0,1].

run_once <- function(n = 100) {
  x <- rnorm(n)
  y <- rnorm(n)          # independent of x  -> true beta1 = 0
  m <- lm(y ~ x)
  # p-value for slope of x
  summary(m)$coef["x", "Pr(>|t|)"]
}

B <- 10000
pvals <- replicate(B, run_once())
type1_rate <- mean(pvals < 0.05) # this is one way of calculating proportion.
type1_rate


# Simulation 2


# install.packages(c("sandwich", "lmtest"))  # if needed
library(sandwich)
library(lmtest)

set.seed(123)

run_once_het <- function(n = 100) {
  x <- rnorm(n)
  # heteroskedastic errors: Var(e | x) increases with |x|
  eps <- rnorm(n, sd = 0.5 + 0.8*abs(x))
  y <- eps                 # true relation: y has NO dependence on x
  m <- lm(y ~ x)

  # Usual OLS p-value (potentially invalid under heteroskedasticity)
  p_default <- summary(m)$coef["x", "Pr(>|t|)"]

  # Robust (heteroskedasticity-consistent) p-value
  p_robust <- coeftest(m, vcov = vcovHC(m, type = "HC1"))["x", "Pr(>|t|)"]

  c(p_default = p_default, p_robust = p_robust)
}

B <- 5000
res <- replicate(B, run_once_het())
mean(res["p_default",] < 0.05)  # Type I with default SE (often > 0.05)
mean(res["p_robust",]  < 0.05)  # Type I with robust SE (≈ 0.05)
type1_rate
hist(pvals, breaks = 40, main = "P-values under true null (homoskedastic)",
     xlab = "p-value")




# install.packages(c("sandwich", "lmtest"))  # if needed
library(sandwich)
library(lmtest)

set.seed(123)

run_once_het <- function(n = 100) {
  x <- rnorm(n)
  # heteroskedastic errors: Var(e | x) increases with |x|
  eps <- rnorm(n, sd = 0.5 + 0.8*abs(x))
  y <- eps                 # true relation: y has NO dependence on x
  m <- lm(y ~ x)
  
  # Usual OLS p-value (potentially invalid under heteroskedasticity)
  p_default <- summary(m)$coef["x", "Pr(>|t|)"]
  
  # Robust (heteroskedasticity-consistent) p-value
  p_robust <- coeftest(m, vcov = vcovHC(m, type = "HC1"))["x", "Pr(>|t|)"]
  
  c(p_default = p_default, p_robust = p_robust)
}

B <- 5000
res <- replicate(B, run_once_het())
# in the following case , too many p_values are smaller than 0.05
# causing us to reject the null hypothesis too many times and think
# there is an association. 

mean(res["p_default",] < 0.05)  # Type I with default SE (often > 0.05)
mean(res["p_robust",]  < 0.05)  # Type I with robust SE (≈ 0.05)

qnorm(0.95)
qnorm(0.75)

pbinom(100,200,0.46)
pnorm(0.4975,0.46,0.03)

pnorm(0.4975,0.46,sqrt(0.46*0.54/200))
