###
### Lab - Multiple Linear Regression 
### By: Farshid Alizadeh-Shabdiz
### Ref: R cookbook chapter 11


library(MASS)
library(ISLR)


# Load the data
names(Boston)
attach(Boston)

colnames(Boston)
summary(Boston)

###########
#Question1:
myModel = lm(medv ~ . , data=Boston)
summary(myModel)

# ANOVA table 
anova(myModel)  

# Model coefficients
coefficients(myModel)
coef(myModel) # Same as coefficients(myModel)

# Confidence interval- Confidence intervals for the regression coefficients
confint(myModel)
confint(myModel, level=.80)

fitted(myModel)   # Vector of fitted y values residuals(m)
residuals(myModel)# Model residuals
resid(myModel)    # Same as residuals(m)
deviance(myModel) # Residual sum of squares

# Key statistics, such as R2, the F statistic, and the residual standard error (σ)
summary(myModel)
# residuals with stats to examine normality - 
#           1Q and 3Q should have almost the same magnitude
# coefficients
#     First column - Least square based coef
#     
# Subset
myModelQ1 = lm(medv ~ age+lstat , subset=1:100 , data=Boston)
summary(myModelQ1)




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

######
# Question 3:
myModelQ2 = lm(medv ~ age + lstat , data=Boston)
summary(myModelQ2)

myModelQ22 = lm(medv ~ . , data=Boston)
summary(myModelQ22)


ind = sample(1:506, 400 ,  replace = FALSE)
myModelQ23 = lm(medv ~ age + lstat , subset= ind , data=Boston)
summary(myModelQ23)

#or
ind = sample(c(TRUE, FALSE), length(lstat), replace=TRUE, prob=c(0.8,0.2))
myModelQ23 = lm(medv ~ age + lstat , subset= ind , data=Boston)

# Predict
testSet = subset(Boston, ind != TRUE)
predict(myModelQ23 ,  data.frame(testSet))

par(mfrow=c(2,2))
plot(myModelQ1)


#######
#Q4 - 1
myModelQ2 = lm(medv ~ . , subset=1:100 , data=Boston)
summary(myModelQ2)

par(mfrow=c(2,2))
plot(myModelQ2)

# How to choose a training set
dim(Boston)
IndTrain = sample(1:506 , 100 , replace=FALSE)
myModelQ1_2 = lm(medv ~ . , subset=IndTrain , data=Boston)
summary(myModelQ1_2)
#Q4 -2:
# Predicting medv^2
# We can force R to calculate something based on the independent variables 
#   before using that in regression
z = medv^2
myModelQ2 = lm(z ~ crim + zn + chas + nox + rm + age + lstat , data=Boston)
# or
myModelQ2_2 = lm(I(medv^2) ~ crim + zn + chas + nox + rm + age + lstat , data=Boston)
summary(myModelQ2)

# Plotting plots many diagnostics models
plot(myModelQ2) # Which provides 4 graphs
# 1. Redidual vs fitted
# 2. Normal Q-Q - normalizes residual and compares that with a 
#    perfect pecentile match, which is going to be a line for Gaussian 
# 3. Scaled location - plots square root of the absolute value of 
#    standardized residuals instead of plotting residuals themselves. Makes graph easier 
# 4. Residual vs leverage - helps to detect outliers in regression. 
# It plots how far covariate of Xi is from the rest of the points. 
#  Basically it measures how sensitive a fitted Y is to a change in yi
# so points with high leverage will be more influntial 
# Cook'd distance: sum of all the changes in the regression model
#      when observation i is removed from it.


plot(myModelQ2, which=1) # Selecting the first one

#Q3
myModelQ3 = lm(medv ~ crim + zn + chas + nox + rm + age + lstat+ I(rm+lstat) , data=Boston)
summary(myModelQ3)
#Q4
myModelQ4 = lm(medv ~ crim + zn + chas + nox + rm + age + lstat + rm*lstat, data=Boston)
summary(myModelQ4)

#Q5
myModelQ5 = lm(medv ~ crim + zn + chas + nox + rm + age + lstat + I(lstat^2), data=Boston)
summary(myModelQ5)

#Extra
# Extract standard error from the model
coef(summary(myModel))[, "Std. Error"]
#Another option
out = summary(myModel)
out$coefficients[ , 2]

# Question 5
myModel5 = lm(medv ~ age + indus, data = Boston)
summary(myModel5)

# Question Bonus
logMedv = log(medv)
modelB = lm (logMedv ~ lstat, data=Boston)

