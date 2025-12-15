library(pROC)

data = read.csv("assignment6_data.csv")
library(dplyr)
# Question 1
data$temp_level = ifelse(data$temp>=98.6,1,0)

# Question 2 
male_data = filter(data,sex ==1)
male_data = select(male_data,temp_level)
female_data = filter(data,sex ==2)
female_data = select(female_data,temp_level)

summary(male_data)
summary(female_data)

boxplot(
  temp_level ~ sex,
  data = data,
  col = c("lightblue", "pink"),
  xlab = "Sex",
  ylab = "Temp Level",
  main = "Temp Level by Sex"
)

# question 3


source("../tests/compare.R")

# given two sample proportions, test if they are equal at at certain alpha level

number_of_males = nrow(subset(data, sex == 1 ))
number_of_females = nrow(subset(data, sex == 2 ))
number_of_positive_in_males = sum(data$sex == 1 & data$temp_level == 1)
number_of_positive_in_females = sum(data$sex == 2 & data$temp_level == 1)

# ----------edit area --------------

sample_size_one = number_of_males
sample_size_two = number_of_females
sample_proportion_one = number_of_positive_in_males/number_of_males
sample_proportion_two = number_of_positive_in_females/number_of_females

alpha = 0.05

# ----------edit area --------------

# H0:population_proportion_one = population_proportion_two

total_positive = sample_proportion_one*sample_size_one + sample_proportion_two*sample_size_two
pooled_sample_proportion = total_positive/(sample_size_one+sample_size_two)
standard_error = sqrt(pooled_sample_proportion*(1 - pooled_sample_proportion)*(1/sample_size_one+1/sample_size_two))

z_statistic = (sample_proportion_one-sample_proportion_two)/standard_error
z_statistic

# -----------testing code --------------------------
# two sided
cat("two sided test\n")

z_critical= qnorm(1-alpha/2)
z_critical
p_value = 2*(1-pnorm(abs(z_statistic)))
p_value 
compareTwoSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)

# -----------question 4 -------------------------
m = glm(temp_level ~ sex,family = binomial,data = data)
summary(m)

alpha = 0.05
z_critical= qnorm(1-alpha/2)

# to calculate the odds ratio and its confidence interval 

# to calculate the odds ratio per x units of difference and its confidence interval
exp(-1 * cbind(OR = coef(m), confint.default(m)))

# make predictions
data$prob <-predict(m, type = "response") # type = "response" asks to calculate probabilities, instead of the linear score


# ROC Curve 
g <- roc(data$temp_level ~ data$prob)

# ---------question 5 --------------

data$sex = as.factor(data$sex )
m2 = glm(temp_level ~ sex + Heart.rate,family = binomial,data = data)
summary(m2)

# to calculate the odds ratio per x units of difference and its confidence interval
exp(-1 * cbind(OR = coef(m2), confint.default(m2)))
exp(10 * cbind(OR = coef(m2), confint.default(m2)))

# make predictions
data$prob <-predict(m2, type = "response") # type = "response" asks to calculate probabilities, instead of the linear score

# ROC Curve 
g2 = roc(data$temp_level ~ data$prob)

# --------question 6------------------

# Plot the ROC Curve. 
plot(g2)

