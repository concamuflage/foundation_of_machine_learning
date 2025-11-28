
# 1 Hypothesis
# H0: Beta1 = 0 or Odds Ratio = 1

# 2 Choose Test statistic
# z_statistic = beta1 / SE(beta1) both can be obtained from the summary(model)
# or it can be obtained from z_value in the summary(model)



data = read.csv("../assignment6/assignment6_data.csv")
library(dplyr)
# Question 1
data$temp_level = ifelse(data$temp>=98.6,1,0)

m = glm(temp_level ~ sex,family = binomial,data = data)
summary(m)

# to calculate the odds ratio and its confidence interval 

# ---------edit area-------------
x = -1 # unit of difference
# ---------edit area-------------


# to calculate the odds ratio per x unit of difference and its confidence interval
exp(difference * cbind(OR = coef(m), confint.default(m)))

#                    OR     2.5 %    97.5 %
#(Intercept) 15.4821429 55.645709 4.3075514
#sex          0.2352941  0.506403 0.1093266
# odds ratio:0.2352941  
# interval: (0.506403 0.1093266)

# to calculate odds ratio per x unit of difference 
exp(m$coefficients[2]*difference )
