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