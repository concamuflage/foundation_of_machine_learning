
# -------------------edit section----------------
dataframe = read.csv("data/assignment4data.csv")

model = lm(dataframe$Prestige.Score ~ 
             dataframe$Education.Level)

alpha = 0.05
number_of_observations = nrow(dataframe)
estimate = 5.361                   # the slope from summary table
standard_error = 0.332             # from the weights standard error in summary
number_of_predictors = 1
summary(model)
anova(model)

# manually calculate the standard error. This result is the same as the one from summary
# ignore this in an exam.
# differences = dataframe$Education.Level - mean(dataframe$Education.Level )
# Sxx = sum(differences^2) # measures the total spread of education level
# standard_error_calculated = sqrt((8287/ n -2)/Sxx)

# ------------------how to read the table -----------------------------

# Coefficients:
#             Estimate     Std. Error       t value         Pr(>|t|)    
#(Intercept)  intercept    SE for intercept 4.71            0.0001 ***
#x            slope        SE for slope     t_statistic     <2e-16 ***



# ---------------F_test ---------------------------

anova_table <- anova(model)
SSreg <- anova_table$"Sum Sq"[1]
SSres <- anova_table$"Sum Sq"[2]

# instead of using the formula, directly use t_value in the summary() is also okay.
t_statistic = estimate/standard_error
t_statistic 

# two sided
cat("two sided test\n")

t_critical = qt(1-alpha/2,number_of_observations -2)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),number_of_observations -2))
p_value 

compareTwoSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)
