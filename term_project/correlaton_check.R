library(car)


data = read.csv("cleaned_data.csv")
head(data)

# --------factorize the categorical columns --------------------
data$race =as.factor(data$race)
is.factor(data$race)
data$gender =as.factor(data$gender)
is.factor(data$gender)
data$marital =as.factor(data$marital)
is.factor(data$marital)


model = glm( coronary ~ age + race +gender+marital+ high_density_level+ low_density_level+ total_cholesterol_level+ triglyceride_level,family = binomial, data = train)
summary(model)
vif(model)
kappa(model.matrix(model))

model = glm( coronary ~ age + race +gender+marital+ high_density_level+ low_density_level+ triglyceride_level,family = binomial, data = train)
summary(model)
vif(model)
kappa(model.matrix(model))
