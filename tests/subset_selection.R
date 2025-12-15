##############################
### Example Subset selection
#Subset selection
library(ISLR)
summary(Hitters)   # Data of baseball players, including Salary which we try to predict here.

Hitters = na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

#Best Subset Regression.
#Using "leaps" library.
library(leaps)
regfit_full = regsubsets(Salary ~ . , data = Hitters) 

# each row represents the best model for that size.
# each entry * means that the best model for that size contains this variable.
summary(regfit_full)

#By default goes to the subset of size 8
#Let us increase that to 19 - all the variables.

regfit_full = regsubsets(Salary ~ . , data = Hitters , nvmax=19) 
reg.summary = summary(regfit_full)

# CP's the the mallow's p
names(reg.summary)          #Get names of the object
plot(reg.summary$cp , xlab="Number of Variables" , ylab = "Cp")
which.min(reg.summary$cp)

# Shows the model corresponding to different CPs. 
# Minimum value at the top is our answer.
# Note that high Cp values are associated to vary small number of parameters.
plot(regfit_full , scale= "Cp") 
coef(regfit_full , 10)