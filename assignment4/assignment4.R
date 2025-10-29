dataframe = read.csv("assignment4data.csv")
par(mfrow = c(1,1))

#question1
plot(
  dataframe$Education.Level,
  dataframe$Prestige.Score,
  ylab = "Prestige Score",
  xlab = "Education Level",
  main = "Education Level vs Prestige Score"
)
cor(dataframe$Prestige.Score,dataframe$Education.Level)
# question 2

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
plot(model3)
