####################
########## ROC curve 
# install.packages("pROC")
# setwd("lectures")

library(pROC)

data<-read.csv("cevent.csv")
m2<-glm(data$event ~ data$chol + data$sex + data$age, family="binomial")

# using model with chol and sex and age 
data$prob <-predict(m2, type = 
                      "response") # type = "response" asks to calculate probabilities.


# ROC Curve 
g <- roc(data$event ~ data$prob)

# Get the Area under the curve
# c-statistics 
g
print(g)

# Plot the ROC Curve. 
plot(g)


# ------------- the following doesn't matter in an exam -----------------------------------------

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


