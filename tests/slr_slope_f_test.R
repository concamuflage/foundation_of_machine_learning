
# -------------------edit section----------------
dataframe = read.csv("data/assignment4data.csv")

model = lm(dataframe$Prestige.Score ~ 
             dataframe$Education.Level)

alpha = 0.05
number_of_observations = nrow(dataframe)

# -----------------------------------------------

# --------------simple F_test--------------------
# just look at the F value and Pr(> F), and decide to reject or not.
# Pr(> F) is the p_value associated with F_statistic
# F value is the F_statistic

anova(model)

# ---------------F_test ---------------------------
number_of_predictors = 1
summary(model)
anova(model)

anova_table <- anova(model)
SSreg <- sum(anova_table$"Sum Sq"[1:number_of_predictors])
SSres <- anova_table$"Sum Sq"[number_of_predictors + 1]

df1 = number_of_predictors
df2 = number_of_observations-number_of_predictors-1

# instead of using the formula, directly use F_statistic in the summary() is also okay.
f_statistic = (SSreg/df1)/(SSres/(df2))
f_statistic 
f_critical = qf(1-alpha,df1,df2)
f_critical
p_value = 1 - pf(f_statistic,df1,df2)
p_value
# f_test can only be right sided
compareRightSided(f_statistic,f_critical)
comparePvalueAlpha(p_value,alpha)

# --------confidence interval----------
confint(model,level = 0.95)



