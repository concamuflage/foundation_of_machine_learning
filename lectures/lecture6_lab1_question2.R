
library(MASS)
library(ISLR)


# Load the data
names(Boston)
attach(Boston)

colnames(Boston)
summary(Boston)

#######
# Simple Linear Regression
#Question 2:
myModel1 = lm(medv ~ lstat , data=Boston)
summary(myModel1)

# F_test and T-test
regss = sum((fitted(myModel1)-mean(medv))^2)
resss=sum(resid(myModel1)^2)
F_test = regss/(resss/504)
F_test
t_test = -0.95005 / 0.03873
t_test 

# t_test squared is equal to F_test
t_test^2

#P-value
1-pf(601, df1=1, df2=504)
pt(-24.53, df=504)*2

# Critical values
qf(0.95, df1=1, df2=504)
qt(0.05/2, df=504)

par(mfrow=c(2,2))
plot(medv, resid(myModel1))
plot(medv, myModel1$residuals)
plot(lstat, myModel1$residuals)
hist(myModel1$residuals)

predict(myModel1, data.frame(lstat = c(2:15)), interval = "confidence")

# 
tmp = lstat^2
myModel11 = lm(medv ~ lstat+ I(lstat^2) , data=Boston)
summary(myModel11)
myModel11 = lm(medv ~ lstat+ tmp , data=Boston)
plot(medv, myModel11$residuals)
plot(lstat, myModel11$residuals)
hist(myModel11$residuals)

par(mfrow=c(1,1))