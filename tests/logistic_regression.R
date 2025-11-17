################################
### lecture 10 code - CS-555
### Regression
### By: Farshid Alizadeh-Shabdiz
#### 


library(ISLR)# Default data
summary(Default)
names(Default)

head(Default$student)
head(Default)
Default$studentBinFlag = ifelse(Default$student=="Yes", 1, 0) # Change students to binary number, no need to convert into a factor.
attach(Default)

glm(default ~ studentBinFlag, data=Default, family=binomial )

#Coefficients:
#   (Intercept)  studentBinFlag  
# -3.5041          0.4049 

# -3.5041 intercept, 
#  0.4049 the slope for studentBinFlag variable.

# Prob for default for a student
exp(-3.5041+0.4049*1)/(1+exp(-3.5041+0.4049))
# Prob for default for a non-student
exp(-3.5041+0.4049*0)/(1+exp(-3.5041+0.4049*0))

# count the total number of students who defaulted.
sum( (Default$student == 'Yes') *(Default$default == 'Yes') ) 
# count the total number of non-students who defaulted.
sum( (Default$student == 'No') *(Default$default == 'Yes') )
# count total number of students who didn't default.
sum( (Default$student == 'Yes') *(Default$default == 'No') )

# the probability calculated is close to the following calculation of probability.
# total number of defaulted students / total numder of students.
127/(127+2817)

# plot summary
# default is marked in red
# default has no relationship with income
# default has a relationship with balance.

plot(balance, income, col=default)

# higher balance, more likely to default.
glm(default ~ balance , data=Default , family=binomial)

# if it is a student, more likely to default.# no need to convert to a numeric
glm(default ~ student , data=Default , family=binomial)

# after adding other variables, the student's coefficient becomes negative. 
# meaning a student is less likely default than a non-student, which contradicts
# the previous model. This is called Simpson's paradox and caused by correlation between
# the predictors.

m= glm(default ~ . , data=Default , family=binomial)
summary(m)


# --------------------------------------------------------------
# Why the 'student' coefficient becomes negative in multivariable logistic regression
# --------------------------------------------------------------

# 1. Correlation between predictors
# - Students tend to have higher balances, and balance is strongly correlated with default.
#   • Students: high balance → more defaults
#   • Non-students: lower balance → fewer defaults
#
# - In the simple model (default ~ student), the 'student' variable acts as a proxy for high balances.
#   It absorbs the effect of balance on default.
#
# - When 'balance' is added to the model, logistic regression separates the effects:
#   • balance → main driver of default risk
#   • student → after controlling for balance, students actually default less often
#
# --------------------------------------------------------------
# 2. Simpson’s paradox in action
# --------------------------------------------------------------
# - When you ignore 'balance', it appears that:
#     "Students default more often."
#
# - But once you hold 'balance' constant, you find that:
#     "Given the same balance, students default less often."
#
# - The overall (aggregate) trend and the within-group trend move in opposite directions
#   because of confounding — 'student' and 'balance' are correlated.
# --------------------------------------------------------------

#########################################################
### Logistic Regression - Module 6, Example 3
# setwd("SET THE Working Director to THE PATH TO THIS DIRECTORY")

# install.packages("aod")
# install.packages("pROC")

library(aod)
library(stats)

setwd("/Users/alizadeh/BostonUniversity/CS555_FoundationsML/OldLectures/R-Examples-master/Datasets")
getwd()

data<-read.csv("cevent.csv")

attach(data)
# print a small part of the data 
head(data)

m <- glm(data$event ~ data$chol, family = "binomial")
summary(m)

m <- glm(data$event ~ ., data=data, family = "binomial")
summary(m)

m <- glm(data$event ~ data$chol, family = "binomial")
summary(m)

# Odd ratio or ORs per 1 unit increase 
# same as calculation by hand (OR): exp(0.02359)

exp(m$coefficients[2])

# build in method for calculating the odds ratio for 
exp(cbind(OR = coef(m), confint.default(m)))



# OR per 10 unit increase 
exp(m$coefficients[2]*10)

# confidence interval for the odds ratio associated with a 10-unit increase in cholesterol.

alpha = 0.05
z_critical = qnorm(1-alpha/2)
standard_error = summary(m)$coefficients[2,2]
exp((m$coefficients[2]-z_critical*standard_error)*10) # lowerbound
exp((m$coefficients[2]+z_critical*standard_error)*10) # higherbound

# The same can be produced by using the following 
exp(10 * cbind(OR = coef(m), confint.default(m)))

### predict function
# predict risk for each patient
risk <-predict(m, type= c("response"))

risk 
# predict risk for patient with chol of 190: exp(-3.12716+0.02119*190)/(1+exp(-3.12716+0.02119*190))
risk[41]

# this is the logistic function
exp(m$coefficients[1] + m$coefficients[2]*190) / (1+exp(m$coefficients[1] + m$coefficients[2]*190))

# You can use predict function for new numbers, Note predict needs new data in the form data frame 
m = glm(default ~ balance , data=Default , family=binomial)

predict(m, newdata = data.frame(balance = 1000) , type=c("response"))
predict(m, newdata = data.frame(balance = c(1000,2000) ), type=c("response")) # predict for multiple points

# The following two ways are the same
# using the student string column
m = glm(default ~ balance+student , data=Default , family=binomial)
mydf = data.frame(balance = 1000, student = "Yes")
predict(m, newdata = data.frame(balance = 1000, student = "Yes") , type=c("response")) # predict for multiple variables

# using the numeric column.

m = glm(default ~ balance+studentBinFlag , data=Default , family=binomial)
mydf = data.frame(balance = 1000, studentBinFlag = 1)
predict(m, newdata = data.frame(balance = 1000, studentBinFlag = 1) , type=c("response")) # predict for multiple variables


# multiple logistic regression 
data$male <- ifelse(data$sex =="M", 1, 0) 
m2<-glm(data$event ~ data$chol + data$male + data$age, family="binomial")
summary(m2)


# ORs per 1 unit increase 
exp(cbind(OR = coef(m2), confint.default(m2)))

# Logistic Regression global test - use wald.test()
?wald.test
wald.test(b=coef(m2), Sigma = vcov(m2), Terms=2:4)