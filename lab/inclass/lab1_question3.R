library(MASS)
library(ISLR)

dataframe = Boston
summary(dataframe)
n = nrow((dataframe))
n
attach(dataframe)
head(dataframe)

model1 = lm(medv ~ age + lstat,data = dataframe)

table = anova(model1)
summary(model1)

# Global Test
SSreg = table[1, "Sum Sq"] + table[2, "Sum Sq"]
SSreg
SSres = table[3, "Sum Sq"]
SSres

f_statistic = (SSreg / 2)/(SSres / 503)
f_statistic

f_critical = qf(0.95,2,503)
f_critical

p_value = 1 - pf(f_statistic, 2,503)
p_value

# since |f_statistic| > f_critical, reject H0. There is significant evidence at alpha = 0.05 showing 
# that at least one of the parameters is not 0. 

# individual test t-test
summary(model1)

# for age
t_statistic_age = 2.826
t_statistic_age
t_critical_age = qt(0.975,492)
t_critical_age

# this parameter is significant

# for lstat 
t_statistic_lstat = -21.416
t_statistic_lstat
t_critical_lstat = qt(0.975,492)
t_critical_lstat 

# this parameter is significant

# second part

# global test
model2 = lm(medv ~ ., data = dataframe)
summary(model2)
df_reg = 13
df_res = 492
f_statistic = 108.1
f_statistic
f_critical = qf(0.95,13,492)
f_critical

# reject the null hypothesis. There is significant evidence at alpha = 0.05 showing that at least one of 
# parameters are significant.

summary(model2)

# if we look p values in the summary, the following ones have a p value
# less than 0.025, so they are significant.

# part 2

# Select 80% of samples randomly and build your model of linear model of medv 
# as a function of age and lstat

n = nrow(dataframe)
sample_size <- n * 0.8
sequence = sample(1:n, sample_size)
train_df = dataframe[sequence,]

model3 = lm(train_df$medv ~ train_df$lstat,train_df$age,data = train_df)

par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(2,2))
plot(model3)


array = c(17,21,20,18,19,22,20,21,16,19)
mean(array)
sd(array)
3/sqrt(10)
qnorm(0.95)

qnorm(0.975)
