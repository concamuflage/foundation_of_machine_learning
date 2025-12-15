
#(1) To get a sense of the data, generate a scatterplot (using an appropriate window, label the axes, and
#title the graph). Consciously decide which variable should be on the x-axis and which should be on the
#y-axis. Using the scatterplot, describe the form, direction, and strength of the association between the
#variables. (4 points)

df <- read.csv("meals_mercury.csv", header = TRUE)
head(df)
degree_freedom = nrow(df)
cat(degree_freedom)

num_meals <- df$meals
mercury_level <- df$mercury

plot(num_meals,
    mercury_level,
    main = "Number of Meals VS Mercury Level",
    xlab = "Number of Meals",
    ylab = " Mercury Levels"
    )
# (2) Calculate the correlation coefficient.  What does the correlation tell us? (2 points)

cor(num_meals,mercury_level)

#(3) Find the equation of the least squares regression equation and write out the equation. 
# Add the regression line to the scatterplot you generated above.  (2 points)
model <- lm(mercury_level ~ num_meals)
coef(model)
abline(model,col = "red", lwd = 2)


#(5) Calculate the ANOVA table AND the table which gives the standard error of the estimates.  
#Formally test the hypothesis that beta1 = 0 using either the F-test or the t-test at the  level.
# Either way, present your results using the 5-step procedure, as described in the course notes.
# Within your conclusion, calculate the R-squared value and interpret this.  
# Also, calculate (using R) and interpret the 90% confidence interval for .
anova(model)
cat("here")
summary(model)


# t value 
qt(0.975,98)

# confidence interval
confint(model,level = 0.95)
