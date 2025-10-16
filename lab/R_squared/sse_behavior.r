# the following code demonstrates that
# As we add more variables, the SS_residual can never increase. 
# In other words, R_squared will only decrease. 
# This is undesired as we shouldn't build better model by adding random variables.
# The solution is adjusted R_squared.

# is the slope along the x1 is the same?
# no, they are different. 
# how to calculate the SS_residual in a space with 3 axis. What is the forumla?


set.seed(42)
n <- 10
x1 <- 1:n
x2 <- rnorm(n)        # random noise, unrelated to y
y  <- 2 + 3 * x1 + rnorm(n, sd = 2)  # y depends only on x1

m1 <- lm(y ~ x1)
m2 <- lm(y ~ x1 + x2) 

sse1 <- sum(resid(m1)^2)
sse2 <- sum(resid(m2)^2)

c(SSE_m1 = sse1, SSE_m2 = sse2)

summary(m1)
summary(m2)