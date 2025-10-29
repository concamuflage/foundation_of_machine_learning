# Example dataset
# (10,20 is influential point)
x <- c(1,2,3,4,10)
y <- c(1,2,2,4,20)


# with influencial point
par(mfrow=c(2,2))
model <- lm(y ~ x)
plot(model)

plot(x,y)
abline(model)

# the outlier's cook's distance is large
cooks <- cooks.distance(model)
cooks
plot(cooks, type="h", main="Cook's Distance", ylab="D_i")
abline(h = 1, col="red", lty=2)

# without influencial point
x <- c(1,2,3,4)
y <- c(1,2,2,4)
model2 <- lm(y ~ x)
plot(x,y,xlim = c(0,10),ylim = c(0,20))
abline(model2)


