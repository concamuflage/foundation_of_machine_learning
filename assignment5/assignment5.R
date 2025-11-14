#setwd("/Users/Mark_1/foundation_of_machine_learning/assignment5")

data = read.csv("student_iq.csv")
head(data)

# question 1
# 1
tapply(data$iq,data$group,length)

# 2
split_data = split(data,data$group)
math_students_dataframe = split_data$`Math student`
physics_student_dataframe = split_data$`Physics student`
chemistry_students_dataframe  = split_data$`Chemistry student`

summary(math_students_dataframe$age)
summary(physics_student_dataframe$age)
summary(chemistry_students_dataframe$age)

summary(math_students_dataframe$iq)
summary(physics_student_dataframe$iq)
summary(chemistry_students_dataframe$iq)

# question2

source("../tests/compare.R")


# if we have multiple groups and we want to know if the means of the groups are different
# due to their group.


# hypothesis:
# h0: all means are equal.
# h1: at least two means are different.

# ------------------- change variables here ------------------------
data = read.csv("student_iq.csv")
alpha = 0.05
total_number_of_groups = length(unique(data$group))
model = aov(data$iq~ factor(data$group),data = data) 
# ------------------------------------------------------------------

total_number_of_observations = nrow(data)
summary(model)

# notes about the summary table

#              Df   Sum Sq        Mean Sq      F value    Pr(>F)    
#data$group     3   (SSB)         (MSB)         21.49    1.1e-05 *** (Between Group)
# Residuals     15   (SSW)        (MSW)                              (Within Group)

# F_value = MSB/MSW 

# --------------Global F_test ---------------------------------------------------------------

f_statistic = summary(model)[[1]]$`F value`[1] # taken from the summary
f_statistic



df_within = total_number_of_observations -total_number_of_groups
df_between = total_number_of_groups - 1

f_critical = qf(1-alpha,df_between,df_within)
f_critical
p_value = 1 - pf(f_statistic,df_between,df_within)
p_value  


# right sided
compareRightSided(f_statistic,f_critical)
comparePvalueAlpha(p_value,alpha)

# Pairwise Test
# the pairwise test if the H0 is rejected

# -----------------change section --------
data = read.csv("student_iq.csv")

vector_value = data$iq
vector_group = data$group
vector_group = factor(vector_group) # make it a factor, especially necessary if the groups are designated by numbers.

# Tukey procedure

model = aov(vector_value ~ vector_group,data = data)
TukeyHSD(model)

# question 3

tapply(data$iq,data$group,mean) # just to know all the levels of the categorical predictor.

# create dummy variables

data$d0 = ifelse(data$group == "Chemistry student",1,0 )
data$d1 = ifelse(data$group == "Math student",1,0 )
data$d2 = ifelse(data$group == "Physics student",1,0 )

# build the models: if a dummy variable is not used, it is the base group
model0 = lm(iq ~ d1 +d2,data = data) 
# to check if the slope is significant/if the differences in mean with the base group is significant
# in other words, check the p_value for each predictor. 
summary(model0)


anova(model0)


# Analysis of Variance Table

#Response: SBP
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
#d1         1  417.31  417.31  9.6550   0.00721 ** 
#d2         1    0.06    0.06  0.0015   0.96975    
#d3         1 2368.82 2368.82 54.8055 2.208e-06 ***
# Residuals 15  648.33   43.22   

# each F_value in this table means:
# "Does this single dummy variable significantly improve the model after accounting for all previous ones?”

# question 4 

# ANCOVA test contains a categorical predictor and multiple continuous predictors.
# 

# import the packages

library(car)
library(emmeans)
library(lsmeans)

# -----------------change section --------------
data = read.csv("student_iq.csv")

vector_value = data$iq # the y value
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



