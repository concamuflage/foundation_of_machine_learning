# when you have two groups and each group have multiple levels.
library(car)

# ----------------Interaction Test ------------------------

data = ToothGrowth
data$supp = as.factor(data$supp)
data$dose = as.factor(data$dose)
levels(data$supp)
levels(data$dose)

model = lm(len ~ supp*dose,data = data)

# we usually use significance level 0.1 for the slope of the interaction term.
summary(model)

# ----------------table interpretation --------------------
#Coefficients:
#                 Estimate Std.Error t value Pr(>|t|)    
#(Intercept)      13.230      1.148  11.521  3.60e-16 ***
#  suppVC         -5.250      1.624  -3.233  0.00209 ** 
#  dose1           9.470      1.624   5.831  3.18e-07 ***
#  dose2          12.830      1.624   7.900  1.43e-10 ***
#  suppVC:dose1   -0.680      2.297  -0.296  0.76831    
#. suppVC:dose2    5.330      2.297   2.321  0.02411 *  
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.631 on 54 degrees of freedom
#Multiple R-squared:  0.7937,	Adjusted R-squared:  0.7746 
#F-statistic: 41.56 on 5 and 54 DF,  p-value: < 2.2e-16

# 2.2e-16 this global f test for all coefficients are significant.
# 0.7746 this model explains 77% of all the variability in y. good model.


# intercepts

# suppVC:dose1 means supplement = VC and dose = 1; this is a cross dummy variable.
# creates cross-product terms of non-baseline dummy variables
# model
# y = beta0 + beta1*suppVC + beta2*dose1 + beta3*dose2+beta4*(suppVC:doese1)+beta5(suppVC:doese2)
# baseline group :supplement = OJ and dose = 0.5

# Coefficient interpretation for lm(len ~ supp * dose, data = ToothGrowth)

# (Intercept) = 13.23
#   Mean tooth length for supplement OJ at dose = 0.5
#   (this is the baseline mean)

# suppVC = -5.25
#   Difference between VC and OJ at dose = 0.5
#   VC group is 5.25 units shorter than OJ at dose 0.5

# dose1 = 9.47
#   Difference between dose = 1 and dose = 0.5 for OJ group
#   Increasing dose from 0.5 to 1 increases length by 9.47 for OJ

# dose2 = 12.83
#   Difference between dose = 2 and dose = 0.5 for OJ group
#   Increasing dose from 0.5 to 2 increases length by 12.83 for OJ

# suppVC:dose1 = -0.68
#   Extra difference for VC vs OJ at dose = 1
#   The VC–OJ difference at dose = 1 is 0.68 less than expected from main effects

# suppVC:dose2 = 5.33
#   Extra difference for VC vs OJ at dose = 2
#   The VC–OJ difference at dose = 2 is 5.33 greater than expected from main effects

#-------------------------------------------------------------------------------------


Anova(model, type=3) # check this one for the p_value of interaction term.

# if there is an crossing between the lines, there is an interaction.
with(data, interaction.plot(dose, supp, len))


# ---------------- if there is an interaction, stratify --------------------

level1 = subset(data,supp == "VC")
level2 = subset(data,supp == "OJ")

# ---------------- then do one way anova global test ------------------------------------

# notes about the summary table

#              Df   Sum Sq        Mean Sq      F value    Pr(>F)    
#data$group     3   (SSB)         (MSB)         21.49    1.1e-05 *** (Between Group)
# Residuals     15   (SSW)        (MSW)                              (Within Group)

# F_value = MSB/MSW 


model1 = aov(len ~ dose, data= level1)
summary(model1)

model2 = aov(len ~ dose, data= level2)
summary(model2)

# proceed to the following step if the Between Group difference is significant.

# --------------------- one way anova pairwise test --------------------------------

# for level 1
vector_value = level1$len
vector_group = level1$dose

# Bonferroni procedure
pairwise.t.test(vector_value,vector_group,p.adjust = "bonferroni",pool.sd = TRUE) 

# Tukey procedure
TukeyHSD(model1)

# for level 2
vector_value = level2$len
vector_group = level2$dose

# Bonferroni procedure
pairwise.t.test(vector_value,vector_group,p.adjust = "bonferroni",pool.sd = TRUE) 

# Tukey procedure
TukeyHSD(model2)



# -----------------Global F_test----------------------------
# H0:there is no effect of either factor 
# H1:one of the factors has an effects


# ----------------Test for first factor -------------------
# H0 ∶ All underlying population means are equal across levels of the first factor, after controlling for the second factor.
# H1: underlying populations means are not equal across levels of the factor tested after controlling for the other. 


#-----------------Test for second factor ------------------
# H0: All underlying population means are equal across levels of the second factor, after controlling for the first factor.
# H1: underlying populations means are not equal across levels of the factor tested after controlling for the other. 