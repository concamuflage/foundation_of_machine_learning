####################
########## ROC curve 
# install.packages("pROC")
# setwd("../tests")

library(pROC)

data<-read.csv("data/cevent.csv")
m2<-glm(data$event ~ data$chol + data$sex + data$age, family="binomial")

# using model with chol and sex and age 
data$prob <-predict(m2, type = 
                      "response") # type = "response" asks to calculate probabilities, instead of the linear score


# ROC Curve 
g <- roc(data$event ~ data$prob)

# Get the Area under the curve
# c-statistics 
g
print(g)

# Plot the ROC Curve. 
plot(g)