# this lab shows how make cases: controls = 1:6 is the optimal in terms of making beta1's variance smallest. 
# after this , adding more control datapoints don't help much in terms of reducing the beta1's variance. 


set.seed(123)

# 1. Simulate a large population with rare events
N <- 200000
x <- rnorm(N)
beta0 <- -4.5   # ~1% prevalence
beta1 <- 2

# true probabilities
p <- 1 / (1 + exp(-(beta0 + beta1 * x)))

# generate outcomes
y <- rbinom(N, size = 1, prob = p)
mean(y)   


# 2. Case indices and control pool
cases <- which(y == 1)
controls_all <- which(y == 0)

length(cases)          # ~2000
length(controls_all)   # ~198000


# 3. Function to run repeated sampling for a given control ratio
estimate_variance <- function(k, reps = 100) {
  
  slope_estimates <- numeric(reps)
  
  for (i in 1:reps) {
    # sample k controls per case
    controls <- sample(controls_all, size = k * length(cases))
    idx <- c(cases, controls)
    
    # fit logistic regression
    fit <- glm(y[idx] ~ x[idx], family = binomial)
    
    slope_estimates[i] <- coef(fit)[2]  # beta1-hat
  }
  
  var(slope_estimates)
}


# 4. Compare different ratios
ratios <- c(1, 2, 3, 4, 5)
variances <- sapply(ratios, estimate_variance)

data.frame(
  Control_Ratio = ratios,
  Variance_of_Beta1 = variances
)

# 4. Compare different ratios
ratios <- c(1,2,3,4,5,6,7,8,9,10)
variances <- sapply(ratios, estimate_variance)

df <- data.frame(
  Control_Ratio = ratios,
  Variance_of_Beta1 = variances
)

# Plot
plot(
  df$Control_Ratio,
  df$Variance_of_Beta1,
  type = "b",          # points + lines
  xlab = "Control-to-Case Ratio",
  ylab = "Variance of Beta1",
  main = "Effect of Control Ratio on Beta1 Variance"
)
