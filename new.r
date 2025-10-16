df <- read.csv("lecture4_example1.csv")
model <- lm(cholosterol ~ age,df)
summary(model)


