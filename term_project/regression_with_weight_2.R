library("haven")
library("survey")
library("jtools")
library("remotes")
library("svrepmisc")
library("dplyr")
library("DT")


result = read.csv("cleaned_data_with_weights.csv")
names(result)


result = subset(result, !is.na(result$weight_2))

design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~weight_2, # using the smaller subsample weight - fasting weight.
  nest = TRUE,
  data = result
)

# filter out columns with missing values. We shouldn't subset on the df directly.
design_sub <- subset(design,
                     !is.na(coronary) &
                       !is.na(triglyceride_level) &
                       !is.na(high_density_level) &
                       !is.na(low_density_level))
nrow(design_sub)

# this model won't converge
model <- svyglm(
  coronary ~  triglyceride_level + high_density_level +low_density_level ,
  design = design_sub,
  family = quasibinomial()
)

# try 1 predictor

design_sub_2 <- subset(design,
                     !is.na(coronary) &
                       !is.na(triglyceride_level))

model <- svyglm(
  coronary ~  triglyceride_level,
  design = design_sub_2,
  family = quasibinomial()
)


