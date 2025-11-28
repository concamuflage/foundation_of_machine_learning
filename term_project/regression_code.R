
# install.packages("caret")

# run logistic regression without considering weights

data = read.csv("cleaned_data.csv")
head(data)

# --------factorize the categorical columns --------------------
data$race =as.factor(data$race)
is.factor(data$race)
data$gender =as.factor(data$gender)
is.factor(data$gender)
data$marital =as.factor(data$marital)
is.factor(data$marital)



# ------------constructing the train, validation, test set ---------------
# each one contains 1/3 of the original data
# in addtion, the proprotion of coronary positive cases are the same.

library(caret)

set.seed(123)   # for reproducibility

# First split: train vs rest (2/3 remain)
index1 <- createDataPartition(data$coronary, p = 1/3, list = FALSE)
train <- data[index1, ]
rest  <- data[-index1, ]

# Second split: validation vs test (each 1/3 of full data)
index2 <- createDataPartition(rest$coronary, p = 1/2, list = FALSE)
validation <- rest[index2, ]
test <- rest[-index2, ]
data$race =as.factor(data$race)
is.factor(data$race)




model = glm( coronary ~ race +gender+marital+ high_density_level+ low_density_level+ total_cholesterol_level+ triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~ race +marital+ high_density_level+ low_density_level+ total_cholesterol_level+ triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~ race + high_density_level+ low_density_level+ total_cholesterol_level+ triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~ high_density_level+ low_density_level+ total_cholesterol_level+ triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~ low_density_level + total_cholesterol_level+ triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~ low_density_level + triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~  triglyceride_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~  high_density_level,family = binomial, data = train)
summary(model)

model = glm( coronary ~ low_density_level,family = binomial, data = train)
summary(model)

