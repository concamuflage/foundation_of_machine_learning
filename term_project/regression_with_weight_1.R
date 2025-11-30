library("haven")
library("survey")
library("jtools")
library("remotes")
library("svrepmisc")
library("dplyr")
library("DT")
library("caret")
library(pROC)


result = read.csv("cleaned_data_with_weights.csv")
names(result)
result = subset(result, !is.na(result$weight_1))

# First split: train vs rest (2/3 remain)
index1 <- createDataPartition(result$SEQN, p = 1/3, list = FALSE)
train <- result[index1, ]
rest  <- result[-index1, ]

# Second split: validation vs test (each 1/3 of full data)
index2 <- createDataPartition(rest$SEQN, p = 1/2, list = FALSE)
validation <- rest[index2, ]
test <- rest[-index2, ]

# make the design object for train, validation,test
design_train <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~weight_1,
  nest = TRUE,
  data = train
)


design_validation <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~weight_1, 
  nest = TRUE,
  data = validation
)

design_test <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~weight_1, 
  nest = TRUE,
  data = test
)

# filter out columns with missing values. 
# We should subset on the design object directly instead of the the dataframe.

design_train_sub <- subset(design_train,
                     !is.na(coronary) &
                       !is.na(high_density_level) &
                       !is.na(low_density_level))
nrow(design_train_sub)

design_validation_sub <- subset(design_validation,
                           !is.na(coronary) &
                             !is.na(high_density_level) &
                             !is.na(low_density_level))
nrow(design_validation_sub)

design_test_sub <- subset(design_test,
                           !is.na(coronary) &
                             !is.na(high_density_level) &
                             !is.na(low_density_level))
nrow(design_test_sub)


# --------- model with two predictors -----------------------
# train model on the train set
model <- svyglm(
  coronary ~ high_density_level +low_density_level ,
  design = design_train_sub,
  family = quasibinomial()
)

summary(model)

# predict on the validation set.

val_df <- design_validation_sub$variables
val_df$predicted <- predict(model, newdata = val_df, type = "response")

roc_val_both <- roc(
  response = val_df$coronary,
  predictor = val_df$predicted,
  weights = val_df$weight_1
)

plot(roc_val_both)

# --------- model with one predictor : high density level -----------------------

# train model on the train set
model <- svyglm(
  coronary ~ high_density_level ,
  design = design_train_sub,
  family = quasibinomial()
)

summary(model)

# predict on the validation set.

val_df <- design_validation_sub$variables
val_df$predicted <- predict(model, newdata = val_df, type = "response")

roc_val_HDL <- roc(
  response = val_df$coronary,
  predictor = val_df$predicted,
  weights = val_df$weight_1
)

plot(roc_val_HDL)

# --------- model one predictor : low density level -----------------------

# train model on the train set
model <- svyglm(
  coronary ~ low_density_level ,
  design = design_train_sub,
  family = quasibinomial()
)

summary(model)

# predict on the validation set.

val_df <- design_validation_sub$variables
val_df$predicted <- predict(model, newdata = val_df, type = "response")

roc_val_LDL <- roc(
  response = val_df$coronary,
  predictor = val_df$predicted,
  weights = val_df$weight_1
)

plot(roc_val_LDL)

roc_val_both
roc_val_HDL
roc_val_LDL

# choose the best threshold for this model and calculate the accuracy. 

roc_obj <- roc(response = val_df$coronary, predictor = val_df$predicted)
best <- coords(roc_obj, "best", best.method="closest.topleft")
best

best_threshold <- as.numeric(best$threshold)

# 2. predicted classes using optimal threshold
val_df$predicted_labels <- ifelse(val_df$predicted >= best_threshold[], 1, 0)

# 3. accuracy
accuracy <- mean(val_df$predicted_labels == val_df$coronary)
accuracy



