dataframe = read.csv("assignment4data.csv")
par(mfrow = c(1,1))

#question1

# (1) To get a sense of the data, generate a scatterplot to examine the association between prestige score
# and years of education. Briefly describe the form, direction, and strength of the association between the
# variables. Calculate the correlation coefficient.
plot(
  dataframe$Education.Level,
  dataframe$Prestige.Score,
  ylab = "Prestige Score",
  xlab = "Education Level",
  main = "Education Level vs Prestige Score"
)
cor(dataframe$Prestige.Score,dataframe$Education.Level)

# question 2

#(2) Perform a simple linear regression with prestige score and years of education, and briefly summarize
#your conclusions (no need to do the 5-step procedure here). Generate a residual plot. Assess whether
#the model assumptions are met. Are there any outliers or influence points? If so, identify them by ID
#and comment on the effect of each on the regression. (5 points)


model1 = lm(dataframe$Prestige.Score ~ dataframe$Education.Level)

abline(model1,col ="red",lwd = 2)

plot(
  model1$fitted.values,
  model1$residuals,
  xlab = "Prestige Score",
  ylab = "Residual",
  main = "Residual Vs Prestige Score"
)
abline(h=0)

par(mfrow =c(2,2))
plot(model1)

# question3 and 4

#(3) Calculate the least squares regression equation that predicts prestige score from education, income,
#and percentage of women. Formally test (using the 5-step procedure) whether the set of these
#predictors are associated with prestige score at the α = 0.05 level (Hint: You should be performing the
# global test).

#(4) If the overall model was significant, summarize the information about the contribution of each
#variable separately at the same significance level as used for the overall model (no need to do a formal
#5-step procedure for each one, just comment on the results of the tests). Provide interpretations for any
#estimates (of the slopes) that are significant. Calculate 95% confidence intervals for any estimates that
#are significant. 

model3 = lm(dataframe$Prestige.Score ~ 
            dataframe$Education.Level +
            dataframe$Income + 
            dataframe$Percent.of.Workforce.that.are.Women)

summary(model3)
alpha = 0.05
n = nrow(dataframe)
k = 3
f_critical = qf(1-alpha,k,n-k-1)
f_critical
confint(model3)
# question 5 

#(5) Generate a residual plot showing the fitted values from the regression against the residuals. Is the fit
# of the model reasonable? Are there any outliers or influence points? (3 points)

plot(model3)
