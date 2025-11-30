
# install.packages("caret")
library(caret)
library(car)
library(pROC)

# run logistic regression without considering weights


data = read.csv("cleaned_data.csv")


# ------------constructing the train, validation, test set ---------------
# each one contains 1/3 of the original data
# in addtion, the proprotion of coronary positive cases are the same.


set.seed(123)   # for reproducibility

# First split: train vs rest (2/3 remain)
index1 <- createDataPartition(data$coronary, p = 1/3, list = FALSE)
train <- data[index1, ]
rest  <- data[-index1, ]

# Second split: validation vs test (each 1/3 of full data)
index2 <- createDataPartition(rest$coronary, p = 1/2, list = FALSE)
validation <- rest[index2, ]
test <- rest[-index2, ]

# -----model with multiple predictors-----------.
# 1. Fit on training set
model <- glm(
  coronary ~ high_density_level + low_density_level + triglyceride_level,
  family = binomial,
  data = train
)

# 2. Predict on *validation* set
validation$prob <- predict(
  model,
  newdata = validation,
  type = "response"
)

# 3. ROC on validation set

g <- roc(response = validation$coronary,
         predictor = validation$prob)

print(g)
plot(g)


# ---------model with one predictor --------------

# --------- 1. Fit model on TRAIN only --------------

model <- glm(
  coronary ~ low_density_level,
  family = binomial,
  data = train
)

# --------- 2. Predict on VALIDATION only -----------

validation$prob <- predict(
  model,
  newdata = validation,
  type = "response"
)

# --------- 3. Compute ROC on VALIDATION -----------

library(pROC)

g <- roc(
  response  = validation$coronary,
  predictor = validation$prob
)

print(g)
plot(g)