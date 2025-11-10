################################
### lecture 10 code - CS-555
### Regression
### By: Farshid Alizadeh-Shabdiz
#### 


library(ISLR)# Default data
summary(Default)
names(Default)

head(Default$student)
Default$studentBinFlag = ifelse(Default$student=="Yes", 1, 0) # Change students to binary number

attach(Default)

glm(default ~ studentBinFlag, data=Default, family=binomial )

# Prob for default for a student
exp(-3.5041+0.4049*1)/(1+exp(-3.5041+0.4049))
# Prob for default for a non-student
exp(-3.5041+0.4049*0)/(1+exp(-3.5041+0.4049*0))

#
sum( (Default$student == 'Yes') *(Default$default == 'Yes') )
sum( (Default$student == 'No') *(Default$default == 'Yes') )
sum( (Default$student == 'Yes') *(Default$default == 'No') )


plot(balance, income, col=default)

glm(default ~ balance , data=Default , family=binomial)

glm(default ~ student , data=Default , family=binomial)


m= glm(default ~ . , data=Default , family=binomial)
summary(m)

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
# same as calculation by hand (OR): exp(0.02119)

exp(m$coefficients[2])

exp(cbind(OR = coef(m), confint.default(m)))



# OR per 10 unit increase 
exp(m$coefficients[2]*10)
exp((m$coefficients[2]-qnorm(0.975)*summary(m)$coefficients[2,2])*10)
exp((m$coefficients[2]+qnorm(0.975)*summary(m)$coefficients[2,2])*10)

# The same can be produced by using the following 
exp(10 * cbind(OR = coef(m), confint.default(m)))

### predict function
# predict risk for each patient
risk <-predict(m, type=c("response"))

risk 
# predict risk for patient with chol of 190: exp(-3.12716+0.02119*190)/(1+exp(-3.12716+0.02119*190))
risk[41]

exp(m$coefficients[1] + m$coefficients[2]*190) / (1+exp(m$coefficients[1] + m$coefficients[2]*190))

# You can use predict function for new numbers, Note predict needs new data in the form data frame 
m = glm(default ~ balance , data=Default , family=binomial)
predict(m, newdata = data.frame(balance = 1000) , type=c("response"))
predict(m, newdata = data.frame(balance = c(1000,2000) ), type=c("response")) # predict for multiple points

m = glm(default ~ balance+studnet , data=Default , family=binomial)
mydf = data.frame(balance = 1000, student = 1)

predict(m, newdata = data.frame(balance = 1000, student = 1) , type=c("response")) # predict for multiple variables

# multiple logistic regression 
data$male <- ifelse(data$sex =="M", 1, 0)
m2<-glm(data$event ~ data$chol + data$male + data$age, family="binomial")
summary(m2)


# ORs per 1 unit increase 
exp(cbind(OR = coef(m2), confint.default(m2)))

# Logistic Regression global test - use wald.test()
?wald.test
wald.test(b=coef(m2), Sigma = vcov(m2), Terms=2:4)


####################
########## ROC curve 
# install.package("pROC")
library(pROC)

# using model with chol and sex and age 
data$prob <-predict(m2, type=c("response"))

# ROC Curve 
g <- roc(data$event ~ data$prob)

# Get the Area under the curve
# c-statistics 
g
print(g)

# Plot the ROC Curve. 
plot(g)

# Just print the results 
roc(data$event ~ data$prob)

# or plot the graphs as well
roc(data$event ~ data$prob, plot=TRUE)

# To get ride of the padding of graph, you can use the par function to set some enviromental variables. 
par(pty="s")

roc(data$event ~ data$prob, plot=TRUE)


# Another way would be to see the x axis as 1-Specificity
plot(1- g$specificities, g$sensitivities, type="l", xlab="1-Specifity", ylab="Sensivity", main="ROC Curve")
abline(a=0, b=1)
grid()


# If you want to have a normal x-axis from zero to one 
roc(data$event ~ data$prob, plot=TRUE, legacy.axes=TRUE)

# if you want to get values in precentages 
roc(data$event ~ data$prob, plot=TRUE, legacy.axes=TRUE, percent=TRUE)

# If you want to understand better specificities and sensitivities. 
# These are just false positive and true negatives. 
roc(data$event ~ data$prob, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive (%)", ylab="True Positive (%)")

# chaning the color 
roc(data$event ~ data$prob, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive (%)", ylab="True Positive (%)", col="blue", lwd=4)


