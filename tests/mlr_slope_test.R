
# -------------------edit section----------------
dataframe = read.csv("data/assignment4data.csv")

model = lm(dataframe$Prestige.Score ~ 
              dataframe$Education.Level +
              dataframe$Income + 
              dataframe$Percent.of.Workforce.that.are.Women)

summary(model)
anova(model)

alpha = 0.05
number_of_observations = nrow(dataframe)
number_of_predictors = 3

# -------------------edit section--------------------------


# ----------global f test ---------

# just inspect the summary table for F_statistic and its associated p-value.
# look for this line: F-statistic: 129.2 on 3 and 98 DF,  p-value: < 2.2e-16
summary(model) 

# -------global f test-------------------------------------

# test if the model is significant
# in other words, if SSreg(explained variance) is far bigger than SSres(unexplained variance)
# h0: all the slopes equal to 0. 


# add all the Sum sq for each weight,excluding residuals
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


# ----------individual slope test ------------------------------------------

# if the null hypothesis is rejected
# check each p value in the summary table. 
# remember these are threshhold when the actual p value is too small. 
# you don't have to these values by two as this is one sided.

summary(model)

# two sided
# slope/ standard_error of the slope.
t_statistic =             # check t_value in the summary(model)

t_critical = qt(1-alpha/2,df)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),df))
p_value 

# ---- calculate confidence interval of the slope ----

confint(model)


