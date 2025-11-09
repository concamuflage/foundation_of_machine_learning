# ANCOVA test contains a categorical predictor and multiple continuous predictors.
# 

# import the packages

library(car)
library(emmeans)
library(lsmeans)

# -----------------change section --------------
data = read.csv("data/smoking_SBP.csv")

vector_value = data$SBP # the y value
vector_group = data$group # the categorical data
vector_group = factor(vector_group) # make it a factor, especially necessary if the groups are designated by numbers.
model = lm(vector_value ~ vector_group + data$age)

# -----------------change section --------------


# for cheacking the 
Anova(model,type = 3)


#               Sum Sq Df  F value Pr(>F)    
# (Intercept)  1689.84  1 5041.084 <2e-16 ***
# vector_group    1.78  3    1.774 0.1982    
# data$age      643.64  1 1920.086 <2e-16 ***
# Residuals       4.69 14  

# 1.774: f_statistic for testing the following hypothesis. 
# H_0: All group means are equal (after adjusting for age)
# 0.1982 is for p_value for the f_statistic
# in this particular case, there are no differences between the groups after adjusting for age.
# this is similar to the global F_test in one way anova.

# 1920.086: f_statistic for testing the following hypothesis. (Remember f_statistic = t_statistic^2)
# H_0: The slope for age = 0 (no relationship between age and SBP)
# in this particular case, age is significant.
# this is similar to test the significance of a slope with F_test in MLR



# ------------ calculating the emmeans and their confidence intervals for each group --------------------------
# use contr.treatment for unordered numeric group
# use contr.poly for ordered numeric group

emm_options(contrast = c("contr.treatment","contr.poly")) 
emmeans(model,specs = "vector_group") 

# -----------------check if the differences in adjusted means are significant -----------------
emmeans(model,specs = "vector_group",contr = "pairwise",adjust = "none")
emmeans(model,specs = "vector_group",contr = "pairwise",adjust = "bonferroni")
emmeans(model,specs = "vector_group",contr = "pairwise",adjust = "tukey")


# ---------alternative way for checking if the differences in adjusted means are significant ---
lsmeans(model,pairwise ~ vector_group, adjust = "none")
lsmeans(model,pairwise ~ vector_group, adjust = "bonferroni")
lsmeans(model,pairwise ~ vector_group, adjust = "tukey")


