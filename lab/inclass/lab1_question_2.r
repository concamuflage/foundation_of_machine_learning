# install.packages("ISLR")
# install.packages("MASS")

library(MASS)
library(ISLR)

dataframe = Boston
attach(Boston)
head(dataframe)
n = nrow(dataframe)
model <- lm(medv ~ lstat)

# hypothesis
# step 1
# h0:slope is 0 
# h1:slope is not 0
# alpha = 0.05 

# calculate the F_value
f_value = qf(0.95,1,n - 2) #3.859975
f_value
anova(model)
f_statistic = 601.62
f_statistic
p_value = 1-pf(f_statistic,1,n-2)
p_value
# since f_static > f_value, we reject the H0. There is significant evidence
# at level 0.05 that the slope is not equal to 0. 

# t_test
# calculate the t_value
t_value = qt(0.975,n -2 ) # 1.964682
cat("t_value",t_value)
# calculate the t_statistic
summary(model)
t_statistics= -24.53
t_statistics
p_value_1 = 2*pt(t_statistics,n-2)
p_value_1
p_value_2 = 2* 2e-16 # this calculation is wrong.2e-16 in the summary is just a threshhold.
p_value_2
# Since |t_statistics | > t_value, we reject the null hypothesis. 


# plot residuals

par(mfrow = c(2,2))
plot(medv,model$residuals)
plot(lstat,model$residuals)
hist(model$residuals)

# make the predictions
predict(model,data.frame(lstat = c(2:15)),interval = "confidence")







