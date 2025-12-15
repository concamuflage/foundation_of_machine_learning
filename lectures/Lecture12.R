################################
### lecture 7 code - CS-555
### Regression
### By: Farshid Alizadeh-Shabdiz
#### 

setwd("/Users/alizadeh/BostonUniversity/CS555_DataAnalysisVisualization/OldLectures/R-Examples-master/Datasets")
getwd()

### Loading Libraries
library(glmnet) 
library(ISLR)
set.seed(555) # makes the experince repeatable

### Data prep
summary(Hitters)   # Data of baseball players, including Salary which we try to predict here.
#Creates a logical vector:
# is.na creastes a vector of TRUE and False.
sum(is.na(Hitters$Salary)) 
Hitters = na.omit(Hitters)
#with make the column Hitters available in the name space.
with(Hitters, sum(is.na(Salary) ))


###############
### Regularizer - Ridge regression and Lasso
    
# expects predictor matrix and response matrix
# -1 :do NOT include an intercept column, which is the default behavior of matrix.glmnet adds its own intercept by default
# ~.: means all other variables in Hitters are predictors 

x = model.matrix(Hitters$Salary ~.-1, data=Hitters)
y = Hitters$Salary


### Ridge regression
# always try many lamdas and save all the corresponding models.
fit.ridge = glmnet(x, y, alpha=0)
#Note: alpha=0 is  Ridge regression
#      alpha=1 is Lasso - Default value
#      alpha <1 is Elastic net

# on this plot, from left to right, lamda decreases. 
# from right to left, lamda increases, and coefficients are pushed to close to 0, but not 0


# xvar="lambda" means use -log(lamda) as x axis.
plot(fit.ridge, xvar="lambda", label=TRUE)

# 100 lamdas are tried for each chosen fold as validation set.
# the main purpose cv.glmnet is to choose the best lamda, which is usually model
# usually lamda.min or lamda.1se
# after choosing the lamda, fit the model with the chosen lamda and return the coefficients.

cv.ridge = cv.glmnet(x, y, alpha=0) # cv.glmnet is the built in cross validation function - default CV K=10.

# for each lamda, we obtained 10 validation errors
# the interval at each lamda shows the 1 sd of the mean
plot(cv.ridge)
coef(cv.ridge)
cv.ridge

# Measure: Mean-Squared Error 

#        Lambda Index Measure    SE  Nonzero
# min     25.5   100  115340  16726      20
# 1se   2023.1    53  130792  13715      20

### Lasso regression
fit.lasso = glmnet(x,y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso = cv.glmnet(x, y) # glmnet built in cross validation. Default is 10 fold.
plot(cv.lasso)
coef(cv.lasso) # some coefficients go to 0, good for feature selection.


### Example - Credit card example
library(ISLR)
summary(Credit)

lm(formula = Balance ~ Student + Limit, data = Credit)

### cross validation - validation set approach
dim(Hitters)

# 180: select 180 indices from the 263 indices/observations. 
# train is a vector of indices.
train = sample(1:263 , 180 , replace=FALSE)
# or
train = sample(seq(263) , 180 , replace=FALSE)
train

# glm always try many lamdas and store the corresponding  models.
lasso.tr = glmnet(x[train,], y[train])
lasso.tr

#.   Df  %Dev  Lambda
# 1   0  0.00 246.300
# 2   2  5.48 224.400

# df = 2 means that 2 coefficients are not 0.
# at the largest lamda, all coefficients are 0. as lamda decreases, more features are used/ more coefficients become non-zero.
# % Dev means how much is explained by the model.

# each column corresponds to one lamda.
# each entry in a column corresponds the prediction with that lamda.
# pred is a matrix
pred = predict(lasso.tr , x[-train,])
dim(pred)


squared_matrix = (y[-train]-pred)^2
# mean is the funtion
# 2 means apply the function to columns of the matrix
mean_squared_error = apply(squared_matrix, 2, mean)
rmse = sqrt(mean_squared_error) # root mean squared error

# from left to right, lamda increases 
plot(log(lasso.tr$lambda), rmse, type="b")
# choose the lamda that has the smallest rmse.
lam.best = lasso.tr$lambda[order(rmse)[1]]
lam.best

coef(lasso.tr, s=lam.best)

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

### Forward subset selection
regfit_fwd = regsubsets(Salary ~ . , data = Hitters, nvmax=19, method ="forward") 
summary(regfit_fwd)

reg.summary = summary(regfit_fwd)
names(reg.summary)          #Get names of the object
plot(reg.summary$cp , xlab="Number of Variables" , ylab = "Cp")
which.min(reg.summary$cp)

plot(regfit_fwd , scale = "Cp")

### Backward subset selection
regfit_bwd = regsubsets(Salary ~ . , data = Hitters, nvmax=19, method ="backward") 
summary(regfit_bwd)

reg.summary = summary(regfit_bwd)
names(reg.summary)          #Get names of the object
plot(reg.summary$cp , xlab="Number of Variables" , ylab = "Cp")
which.min(reg.summary$cp)

plot(regfit_bwd , scale = "Cp")


# Cross validation - Model selection using a validation set.
#---------------------------------------
dim(Hitters)
train = sample(seq(263) , 180 , replace=FALSE)
train
regfit.fwd = regsubsets(Salary ~ . , data = Hitters[train,], nvmax=19, method="forward")

val.errors = rep(NA,19)
# construct a matrix with test set, and add intercept column
x.test = model.matrix(Salary~. , data=Hitters[-train,])

# Predict by hand, since there is no predict method.
for (i in 1:19){ # loop over model sizes
  coefi = coef(regfit.fwd , id=i) # extract coefficients for given model size
  pred = x.test[,names(coefi)] %*% coefi  # %*% is matrix multiplication
  val.errors[i] = mean((Hitters$Salary[-train]-pred)^2)
}

plot(sqrt(val.errors) , ylab="RMSE" , ylim=c(300,400) , pch=19 , type="b") # Validation points
points(sqrt(regfit.fwd$rss[-1]/180) , col="blue" , pch=19 , type="b")     # Training points

### Extra - writing predict as a function
# We can also write predict as a function - which needs to know regsubsets class "call" 
predict.regsubsets = function(object , newdata, id, ...){
  form = as.formula(object$call[[2]] )
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  mat[,names(coef)] %*% coefi
}
### End of Extra


#############################################################
# Cross validation
# 10 fold cross validation

folds = sample(rep(1:10 , length = nrow(Hitters)))
folds
table(folds)

cv.errors = matrix(NA , 10, 19)
### Note following returns ERROR, since there is no predict method for regsubsets
for (k in 1:10){
  best.fit = regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
  for (i in 1:19){
    pred = predict(best.fit, Hitters[folds==k], id=i)
    cv.errors[k,i] = mean( (Hitters$Salary[folds==k]-pred)^2)
  }
} 
### ERROR!

for (k in 1:10){
  best.fit = regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
  for (i in 1:19){
    coefi = coef(best.fit , id=i)
    x.test = model.matrix(Salary~. , data=Hitters[folds==k,])
    pred = x.test[,names(coefi)] %*% coefi
    cv.errors[k,i] = mean( (Hitters$Salary[folds==k]-pred)^2)
  }
} 

rmse.cv = sqrt(apply(cv.errors,2,mean))
plot(rmse.cv , pch=19, type="b")


