
library(pROC)

# convert categorical variable to factor is necessary

data = read.csv("../assignment6/assignment6_data.csv")
library(dplyr)
# Question 1
data$temp_level = ifelse(data$temp>=98.6,1,0)


data$sex = as.factor(data$sex ) # this step is necessary.
m = glm(temp_level ~ sex + Heart.rate,family = binomial,data = data)
summary(m2)

# to calculate the odds ratio per x units of difference and its confidence interval
difference = 10
exp(difference * cbind(OR = coef(m), confint.default(m)))

# calculate manually

# OR per - unit increase 
exp(1.38919*-1)
exp(0.06337*10)
