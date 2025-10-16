# x1,x2,x3 are perfectly collinear and 
# x2,x3 are not used in constructing the model.
# R_squared stays the same.


# Example data
x1 <- c(1, 2, 3, 4, 5)
x2 <- c(2, 4, 6, 8, 10)
x3 <- c(3,6,9,12,15)
y  <- c(1.9, 4.2, 5.8, 8.5, 10.3)

# Model 1: 1 predictor
m1 <- lm(y ~ x1)
summary(m1)$r.squared

# Model 2: Add a redundant predictor
m2 <- lm(y ~ x1 + x2)
summary(m2)$r.squared

# Model 2: Add a redundant predictor
m3 <- lm(y ~ x1 + x2 + x3)
summary(m3)$r.squared