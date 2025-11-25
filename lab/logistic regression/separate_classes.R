# when two classes are completely separate
# The blue dots and reds are separable by a vertical line.
# so the slope will try to go to positive infinity, intercept tries to go to negative infinity
# we have big standard error. 
# 
set.seed(1)

# Generate completely separated data
n <- 40
x <- rnorm(n)

# Create perfectly separated classes:
# If x > 0 → Y = 1
# If x < 0 → Y = 0
y <- ifelse(x > 0, 1, 0)

plot(x, y, pch=19, col=ifelse(y==1, "red", "blue"),
     main="Complete Separation Example",
     xlab="x", ylab="y")

# Fit logistic regression
model <- glm(y ~ x, family = binomial)

summary(model)


# --------second example --------------

set.seed(2)

# Two clusters separated in x
x1 <- rnorm(20, mean = -2)
x2 <- rnorm(20, mean =  2)

x <- c(x1, x2)
y <- c(rep(0, 20), rep(1, 20))

plot(x, y, pch=19, col=ifelse(y==1, "red", "blue"),
     main="Complete Separation Example",
     xlab="x", ylab="y")

model <- glm(y ~ x, family = binomial)
summary(model)