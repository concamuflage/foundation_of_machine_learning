# doing one way anova as MLR
# the slopes are differences in means with the base group
# H0: all the slopes are 0. (In other words, the means of all the groups are the same.)

data = read.csv("data/smoking_SBP.csv")
attach(data)
head(data)
tapply(data$SBP,data$group,mean) # just to know all the levels of the categorical predictor.

# create dummy variables

data$d0 = ifelse(data$group == "Current heavy smoker",1,0 )
data$d1 = ifelse(data$group == "Current light smoker",1,0 )
data$d2 = ifelse(data$group == "Former smoker",1,0 )
data$d3 = ifelse(data$group == "Never smoker",1,0 )

# build the models: if a dummy variable is not used, it is the base group
model0 = lm(SBP ~ d1 +d2 +d3,data = data) 
# to check if the slope is significant/if the differences in mean with the base group is significant
# in other words, check the p_value for each predictor. 
summary(model0)
anova(model0)


> anova(model0)
Analysis of Variance Table

#Response: SBP
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
#d1         1  417.31  417.31  9.6550   0.00721 ** 
#d2         1    0.06    0.06  0.0015   0.96975    
#d3         1 2368.82 2368.82 54.8055 2.208e-06 ***
# Residuals 15  648.33   43.22   

# each F_value in this table means:
# "Does this single dummy variable significantly improve the model after accounting for all previous ones?”

# check the confidence interval for the difference/slope
confint(model0, level = 0.95)

