###
# CS 555 review

#1. Central Limit Theorem
#Dataset: faithful (Old Faithful geyser data)
#Idea: Take many random samples of waiting times, look at the distribution of sample means.

data(faithful)
set.seed(1)

# draws from waiting column 10 samples and calculate the mean
# repeat 1000 times the previous step, so we have 1000 means.
# means_10 is a vector of means
means_10  <- replicate(1000, mean(sample(faithful$waiting, size = 10, replace = TRUE)))
hist(means_10, main = "CLT: Sample Means (n = 10)")

means_50  <- replicate(1000, mean(sample(faithful$waiting, size = 50, replace = TRUE)))
hist(means_50, main = "CLT: Sample Means (n = 50)")

####
# Normal Distribution
#Dataset: faithful
#Idea: Show that eruptions is approximately normal (or not) via histogram and Q–Q plot.

data(faithful)
# breaks = 20 is the bin size
hist(faithful$eruptions, breaks = 20, probability = TRUE,
     main = "Eruption Duration", xlab = "Minutes")
# dnorm calculates the density at each x^i, not the probability
# add means draw on top of the histogram
# x is not defined. curve will define it. it takes the x_axis of the histogram.
curve(dnorm(x, mean(faithful$eruptions), sd(faithful$eruptions)),
      add = TRUE)

qqnorm(faithful$eruptions); 
qqline(faithful$eruptions)


#####
# Confidence Interval for a Proportion
#Dataset: mtcars (manual vs automatic transmission)
# Treat am (0 = automatic, 1 = manual) as a Bernoulli variable.
data(mtcars)

x <- sum(mtcars$am == 1)   # number of manuals
n <- nrow(mtcars)
pr<- x/n
pr+qnorm(0.05/2)*sqrt(pr*(1-pr)/n)
pr-qnorm(0.05/2)*sqrt(pr*(1-pr)/n)

# following is a better way to calculate CI
prop.test(x, n, correct = FALSE)  # CI for proportion of manuals - 


####
#Two-Sample MEAN Test (Two-Sample t-Test)
#Dataset: mtcars
#Compare mean mpg between automatic and manual cars.
t.test(mpg ~ am, data = mtcars, var.equal = TRUE)

####
#Correlation
#Dataset: mtcars
#Correlation between mpg and wt.
cor(mtcars$mpg, mtcars$wt)
plot(mpg ~ wt, data = mtcars)
abline(lm(mpg ~ wt, data = mtcars), col = 2)

####
#Multiple Linear Regression
#Dataset: mtcars
#Predict mpg from several predictors.

mlr_model <- lm(mpg ~ wt + hp + disp, data = mtcars)
summary(mlr_model)
anova(mlr_model)

####
#Polynomial Multiple Regression
#Dataset: women (height and weight)
# relationship is nonlinear; add a quadratic term.
data(women)

# I() just mean interpret ^ as an operator. I mean as is. Don't treat it specially.
poly_model <- lm(weight ~ height + I(height^2), data = women)
summary(poly_model)

plot(women$height, women$weight)
# x is a vector of numbers chosen by curve. it is from the numbers in plot x_axis.
# we see the predicted value and real values are pretty close.
curve(predict(poly_model, newdata = data.frame(height = x)),
      add = TRUE)

####
#Interaction Models
#Dataset: mtcars
#Effect of weight on mpg depends on transmission type (am).
int_model <- lm(mpg ~ am+wt+ wt*am, data = mtcars)
summary(int_model)

####
#Categorical Predictors (Dummy Variable Regression)
#Dataset: mtcars
#Use number of cylinders as a factor.
cat_model <- lm(mpg ~ factor(cyl), data = mtcars)
summary(cat_model)
#This creates dummy variables for cyl = 6 and cyl = 8 (with 4 as baseline).
# the prediction is just the sample mean difference of each group with base.
# given a category, its prediction = the sample mean of the group.
cyl8 = subset(mtcars,cyl == 8, mpg)
cyl4 = subset(mtcars,cyl == 4, mpg)
mean(cyl8$mpg) - mean(cyl4$mpg) # this is exactly the second beta.

####
#11. Ridge Regression (L2 Regularized MLR)
#Dataset: mtcars, with glmnet package.
#Predict mpg from all other variables.
#install.packages("glmnet")  # once
library(glmnet)

data(mtcars)
# -1: drop the intercept column
x <- model.matrix(mpg ~ . , data = mtcars)[, -1]  # predictors matrix
y <- mtcars$mpg

# find the best lamda
lambdaVec = cv.glmnet(x,y, alpha=0)
lambdaVec$lambda.min 
# fit with the best lamda
ridge_fit <- glmnet(x, y, alpha = 0 , lambda = lambdaVec$lambda.min)  # alpha = 0 → Ridge
print(ridge_fit$beta)

####
#12. Lasso Regression (L1 Regularized MLR)
#Dataset: mtcars, again with glmnet.
lambdaVec = cv.glmnet(x,y, alpha=1)
lambdaVec$lambda.min 

lasso_fit <- glmnet(x, y, alpha = 1, lambda = lambdaVec$lambda.min)  # alpha = 1 → Lasso
print(lasso_fit$beta)

####
#13. Weighted Least Squares (WLS)
#Dataset: airquality
#Example: variance of Ozone may depend on Wind; use weights inversely related to Wind².
data(airquality)
aq <- na.omit(airquality)

ols <- lm(Ozone ~ Temp, data = aq)
wls <- lm(Ozone ~ Temp, data = aq, weights = 1 / (Wind^2))

summary(ols)
summary(wls)

####
#14. Logistic Regression
#Dataset: mtcars
#Model probability of manual transmission (am) given car features.
logit_model <- glm(am ~ wt + hp, data = mtcars, family = binomial)
summary(logit_model)

####
#15. ANOVA (General One-Way)
#Dataset: PlantGrowth
#Compare mean plant weight across 3 treatment groups.
data(PlantGrowth)

anova_model <- aov(weight ~ group, data = PlantGrowth)
summary(anova_model)

####
#16. ANCOVA
#Dataset: iris
#Effect of species on Sepal.Length, adjusting for Sepal.Width (covariate).
data(iris)

ancova_model <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)
anova(ancova_model)   # Type I ANOVA table
summary(ancova_model)


library(car)
Anova(ancova_model, type=3)

####
#17. One-Way and Two-Way ANOVA
#One-Way ANOVA – PlantGrowth (already above):
oneway <- aov(weight ~ group, data = PlantGrowth)
summary(oneway)
#Two-Way ANOVA – warpbreaks
#Factors: wool type (wool), tension (tension).
data(warpbreaks)

twoModel = lm(breaks ~ wool + tension + wool*tension, data = warpbreaks)
summary(twoModel)

twoway <- aov(breaks ~ wool * tension, data = warpbreaks)
summary(twoway)



