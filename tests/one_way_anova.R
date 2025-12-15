source("compare.R")

# if we have multiple groups and we want to know if the means of the groups are different
# due to their group.


# hypothesis:
# h0: all group means are equal.
# h1: at least two means are different.

# ------------------- change variables here ------------------------
data = read.csv("data/smoking_SBP.csv")
alpha = 0.05
total_number_of_groups = length(unique(data$group))
model = aov(data$SBP~ factor(data$group),data = data) 
# ------------------------------------------------------------------

# do f_test without doing calculations, look at the Pr(>F) value in the following table.
# is it bigger than the alpha or smaller? that is it! 
# f_statistic is the F value in the table.

anova(model)

# Analysis of Variance Table

#Response: data$SBP
#                    Df  Sum Sq Mean Sq F value    Pr(>F)    
#factor(data$group)  3  2786.19  928.73  21.487 1.098e-05 ***
#Residuals          15  648.33   43.22                      
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  
# SSBetween = 2786.19
# SSWithin = 648.33
# MSBetween = 928.73
# MSWithin = 43.22

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








