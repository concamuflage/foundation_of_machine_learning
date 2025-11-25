################################
### lecture 11 code - CS-555
### Poly Regression

### Farshid Alizadeh-Shabdiz
### Reference: intro to statistical learning, G. James, et. al. 
#### 


library(ISLR)# Default data
attach(Wage)

# Linear vs quadratic function
fit0 = lm(wage ~ age , data=Wage)
summary(fit0)

fit0a=lm(wage ~ age+I(age^2), data=Wage)
summary(fit0a)

# Degree of 4 implementation
fit = lm(wage ~ poly(age,4, raw=TRUE) , data=Wage)
summary(fit)
coef(summary(fit))

fita=lm(wage ~ age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fita)
coef(fita)

fitb=lm(wage ~ cbind(age,age^2,age^3,age^4),data=Wage)

# Plot the data and add the fit
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit )

#par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey")
title("Degree -4 Polynomial ",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Chech higher polynomial - up to degree 6
fit6b=lm(wage ~ age+I(age^2)+I(age^3)+I(age^4)+I(age^5)+I(age^6),data=Wage)
summary(fit6b)

fit6c = lm(wage ~ poly(age,6, raw=TRUE) , data=Wage)
summary(fit6c)

## EXTRA
# comparing polynomial models of different degrees at once. poly(age, 6) is a matrix
# By default, raw = FALSE. Then poly() computes an orthogonal polynomial and 
# scales the columns so that each column is orthogonal to the previous ones
fit6 = lm(wage ~ poly(age,6) , data=Wage)
summary(fit6)


# ANOVA
# fit five different models and sequentially compare the simpler 
#model to the more complex model.
fit.1=lm(wage ~ age,data=Wage)
fit.2=lm(wage ~ poly(age,2),data=Wage)
fit.3=lm(wage ~ poly(age,3),data=Wage)
fit.4=lm(wage ~ poly(age,4),data=Wage)
fit.5=lm(wage ~ poly(age,5),data=Wage) 

anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# Or can be used regression p_values
coef(summary(fit.5))
#Notice that the p-values are the same, and in fact the square of 
#the t-statistics are equal to the F-statistics from the anova() 
#function; for example:
(-11.983) ^2

# Logistic Regression
# consider the task of predicting whether an individual earns more 
# than $250,000 per year. 
#create the appropriate response vector, and then apply the glm() 
#function using family="binomial".
fit=glm(I(wage>250) ~ poly(age,4),data=Wage,family=binomial)
# note: following command also can be used to create binary wage
wageBinary = ifelse(wage > 250, 1, 0)

summary(fit)

# This means we get predictions for the logit 
preds = predict(fit,newdata=list(age=age.grid),se=T)
# or
preds = predict(fit,newdata=list(age=age.grid))
preds

# directly computed the probabilities by selecting the type="response" 
# option in the predict() function.
preds=predict(fit,newdata=list(age=age.grid),type="response", se=T)
preds


# Splines
library(splines)
# The bs() function generates the entire matrix of basis functions
# for splines with the specified set of knots. 
#By default, cubic bs() splines are produced.
fit=lm(wage ~ bs(age,knots=c(25,40,60)),data=Wage)

pred=predict(fit,newdata=list(age=age.grid),se=T)

plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se ,lty="dashed")
lines(age.grid,pred$fit-2*pred$se ,lty="dashed")

# In order to instead fit a natural spline, we use the ns() function.
fit2=lm(wage ~ ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

# Smoothed spline
plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey")
title (" Smoothing Spline ")
fit=smooth.spline(age , wage, df=16)
fit2=smooth.spline(age , wage, cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


