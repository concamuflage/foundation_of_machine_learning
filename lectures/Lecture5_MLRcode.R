################################
### lecture 6 code - CS-555
### Regression
### By: Farshid Alizadeh-Shabdiz
#### 

setwd("/Users/alizadeh/BostonUniversity/CS555_FoundationsML/OldLectures/R-Examples-master/Datasets")
getwd()

# setwd("SET THE Working Director to THE PATH TO THIS DIRECTORY")

## class example SLR
data1 <- read.csv("CEO_salary.csv")
attach(data1)
summary(data1)

salary <- salary/1000

plot(salary, age)

plot(salary, height)

pairs(data1)

m_slr = lm(salary ~ age)
summary(m_slr)
plot(age, salary)
abline(m_slr)

m_slr2 = lm(salary ~ height)
summary(m_slr2)
plot( height , salary )
abline(m_slr2)

##### MLR
### Example 1 - CEO salaries
data <- read.csv("CEO_salary.csv")
attach(data)

# Divid the Salary by 1000 to be able to better show the values.
salary1 <- salary/1000

# Now we create a New Data frame out of age, height and Salary
data1 <- data.frame(age, height, salary1)

plot(salary1, age)

plot(salary1, height)

# You can also the following command to replace the column inside the original dataframe.
# data[,"salary"] <- salary1

# Calulate Correlation Coefficient
cor(data1)

# Plot Scatterplot Matrix
pairs(data1)

# Fit a Multiple Linear Regression model into data.
# Variables are Salary and age
lm(formula = salary1 ~ age + height)

# or better store the resulted model into a variable for further use.

m <- lm(salary1~age+height)

# Summar function can calculate almost everything that you need.
summary(m)

# Calulate R squared Manually and the P-Value

# Total Sume of Squared.
totalss <- sum((salary1 - mean(salary1))^2)

# Regression and Residual Sum of the Squered.
regss <- sum((fitted(m) - mean(salary1))^2)
resiss <- sum((salary1-fitted(m))^2)

# Calulate the F Value.
fstatistic <- (regss/2)/(resiss/97)

# Calculate critical value
qf(0.95,df1=2, df2=97)

# The P-Value for F-Statistic.
pvalue <- 1-pf(fstatistic, df1=2, df2=97)

# Calulate R squared.
R2 <- regss/totalss
R2
# Regression Diagnostics
# Residual Plots
resid(m)
par(mfrow=c(2,2))


plot(age, resid(m), axes=TRUE, frame.plot=TRUE, xlab='age', ylab='residue')
plot(height, resid(m), axes=TRUE, frame.plot=TRUE, xlab='height', ylab='residue')

plot(fitted(m), resid(m), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')

hist(resid(m))

confint(m , level=0.99)


### Example 2 - States poverty
# Read the data first
states <- read.csv("https://raw.githubusercontent.com/kiat/R-Examples/master/Datasets/states.csv")

attach(states)

# data <-data.frame(poverty, metro_res, white, hs_grad, female_house)


m <- lm(poverty~female_house+white)

# Summar function can calculate almost everything that you need. 
summary(m)

# Using anova table 
anova_table <-anova(m)
anova_table

SSE <-anova_table$`Sum Sq`[3]
SSE

SST <-anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]+anova_table$`Sum Sq`[3]
SST

R2 <- (anova_table$`Sum Sq`[1]+anova_table$`Sum Sq`[2]) / SST
R2
# adjusted R^2
# adjusted_R2 <-  1 - (SSE/SST )  * (( n - 1 ) / ( n - k - 1) )
# n number of samples
# k number of independent varaibles 

adjusted_R2 <-  1 - (SSE/SST )  * (( length(poverty) - 1 ) / ( length(poverty) - 2 - 1) )


# Second model - Simple Linear Regression Only considering the female_house 
m2 <- lm(poverty~female_house)
anova_table2<-anova(m2)
anova_table2

totalss_anova2 <-anova_table2$`Sum Sq`[1]+anova_table2$`Sum Sq`[2]
totalss_anova2

# Third  model - Simple Linear Regression Only considering the white
m3 <- lm(poverty~white)

summary(m3)
anova_table3<-anova(m3)
anova_table3

totalss_anova3 <-anova_table3$`Sum Sq`[1]+anova_table3$`Sum Sq`[2]
totalss_anova3


#### Example 3 - CEO data with matrix calculation
# Matrix Operations in R  https://www.statmethods.net/advstats/matrix.html 
# Some good example https://datascienceplus.com/linear-regression-from-scratch-in-r/


data <- read.csv("CEO_salary.csv")
salary1 <- data$salary/1000

y <- salary1
X <- as.matrix(cbind(data$age, data$height))

# vector of ones with same length as rows in y
int <- rep(1, length(y))

# Add intercept column to X
X <- cbind(int, X)

# Implement closed-form solution
betas <- solve(t(X) %*% X) %*% t(X) %*% y  # Note solve() is the standard R command for inverse matrix

# Round for easier viewing
betas <- round(betas, 2)

print(betas)



# Linear regression model
lm.mod  <- lm(salary1~ data$age + data$height)

# Round for easier viewing
lm.betas <- round(lm.mod$coefficients, 2)

# Create data.frame of results
results <- data.frame(our.results=betas, lm.results=lm.betas)

print(results)

# anova function using different base model
anova(m2 , m)



