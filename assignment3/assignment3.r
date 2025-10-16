
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

cor(num_meals,mercury_level)

model <- lm(mercury_level ~ num_meals)
coef(model)
abline(model,col = "red", lwd = 2)

anova(model)
cat("here")
summary(model)

# t value 
qt(0.975,98)

# confidence interval
confint(model,level = 0.95)