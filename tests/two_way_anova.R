# when you have two groups and each group have multiple levels.
library(car)

# Step 1 Interaction Test
## if no interaction, do the ANCOVA
## else: stratify by one factor and do ONE-WAY anova.

# ----------------Interaction Test ------------------------

data = ToothGrowth
data$supp = as.factor(data$supp)
data$dose = as.factor(data$dose)
levels(data$supp)
levels(data$dose)

model = lm(len ~ supp*dose,data = data)

# check this one for the p_value of interaction term.
# compare it with 0.01. if the 2* (Pr(>F)  for the interaction term) is smaller than 0.01, then the slope 
# is significant, and we have an interaction. Else, we don't. 

Anova(model, type=3) 

# if there is no interaction, check the summary table for ANCOVA.

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


#------------------ if there is no interaction, reconstruct the model without the interactioin term ----
ancova_model = lm(len ~ supp + dose,data = data)
Anova(ancova_model, type=3) 


#              Sum Sq  Df F value    Pr(>F)    
#(Intercept)   2326.91  1 158.828 < 2.2e-16 ***
#  supp         205.35  1  14.017 0.0004293 ***
#  dose        2426.43  2  82.811 < 2.2e-16 ***
#  Residuals    820.43 56     
# supp: After controlling for dose, the mean tooth length differs significantly between supplement types (OJ vs VC)
# 205.35:Variation in len explained by supplement type,after adjusting for dose.

summary(ancova_model)

#Coefficients:
#             Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)  12.4550     0.9883   12.603  < 2e-16 ***
#suppVC       -3.7000     0.9883   -3.744  0.000429 ***
#dose1         9.1300     1.2104   7.543   4.38e-10 ***
# dose2        15.4950     1.2104  12.802  < 2e-16 ***

#Residual standard error: 3.828 on 56 degrees of freedom
#Multiple R-squared:  0.7623,	Adjusted R-squared:  0.7496 
#F-statistic: 59.88 on 3 and 56 DF,  p-value: < 2.2e-16
# base: suppOJ, dose0.5
# Intercept = mean at OJ + dose 0.5, All coefficients are differences from that baseline
# 12.455 Mean tooth length for OJ supplement at dose 0.5
# -3.7 At the same dose, VC produces teeth that are on average 3.7 units shorter than OJ
# 9.13 Increasing dose from 0.5 → 1.0 increases tooth length by ~9.13 units(averaged across supplements)
# 15.49 Increasing dose from 0.5 → 2.0 increases tooth length by ~15.50 units((averaged across supplements))
# f_statitic and p value:At least one predictor significantly explains variation in tooth length (H0:H_0:all slopes are zero})
# 0.7623:~76% of variability in tooth length is explained by supplement + dose
# 3.828:This is the within-group variability after accounting for supp and dose.

# ---------------- if there is an interaction, stratify --------------------

level1 = subset(data,supp == "VC")
level2 = subset(data,supp == "OJ")

# ---------------- then do one way anova global test ------------------------------------

# do global f_test without doing calculations, look at the Pr(>F) value in the following table.
# is it bigger than the alpha or smaller? that is it! 
# f_statistic is the F value in the table.

model1 = aov(len ~ dose, data= level1)
model2 = aov(len ~ dose, data= level2)
anova(model1)
anova(model1)

##ignore in an exam, this approach gets the f_statistic from the summary table 
#summary(model1)
#summary(model2)
# notes about the summary table

#              Df   Sum Sq        Mean Sq      F value    Pr(>F)    
#data$group     3   (SSB)         (MSB)         21.49    1.1e-05 *** (Between Group)
# Residuals     15   (SSW)        (MSW)                              (Within Group)

# F_value = MSB/MSW 
##ignore

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
# H0∶ All underlying population means are equal across levels of the first factor, after controlling for the second factor.
# H1: underlying populations means are not equal across levels of the factor tested after controlling for the other. 


#-----------------Test for second factor ------------------
# H0: All underlying population means are equal across levels of the second factor, after controlling for the first factor.
# H1: underlying populations means are not equal across levels of the factor tested after controlling for the other. 