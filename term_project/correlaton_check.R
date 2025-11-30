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

# removed total cholesterol because it is calculated from high and low, which indicates high collinearity.
# 
model = glm( coronary ~ age + race +gender+marital+ high_density_level+ low_density_level+ triglyceride_level,family = binomial, data = train)
summary(model)
vif(model)
kappa(model.matrix(model))

# removing all demographic variables
model = glm( coronary ~ high_density_level+ low_density_level+ triglyceride_level,family = binomial, data = train)
summary(model)
vif(model)
kappa(model.matrix(model))



# Get the Area under the curve
# c-statistics 
data$prob <-predict(model, type = 
                      "response") # type = "response" asks to calculate probabilities, instead of the linear score


# ROC Curve 
g <- roc(data$coronary ~ data$prob)
print(g)

# Plot the ROC Curve. 
plot(g)

# Conclusion: This is a very bad model. Need to consider the weights, because there is oversampling. 

# ---------use one predictor --------------

# removing all demographic variables
model = glm( coronary ~ low_density_level,family = binomial, data = train)

# Get the Area under the curve
# c-statistics 
data$prob <-predict(model, type = 
                      "response") # type = "response" asks to calculate probabilities, instead of the linear score


# ROC Curve 
g <- roc(data$coronary ~ data$prob)
print(g)

# Plot the ROC Curve. 
plot(g)

# Conclusion: This is a very bad model. Need to consider the weights, because there is oversampling. 

