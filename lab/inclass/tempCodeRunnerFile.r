library(MASS)
library(ISLR)

dataframe <- MASS :: Boston
head(dataframe)

model <- lm(dataframe$medv ~ dataframe$lstat)

anova(model)
summary(model)