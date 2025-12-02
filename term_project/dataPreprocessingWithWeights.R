# The main differences of this version of data processing is that it didn't filter out rows with empty values.
# This part is left to filtering in the survey object. 

# Important

# WTSAF2YR is for a smaller subsample. WTSAF2YR is none_zero if a person is chosen to be in the fasting subsample and did fast before providing blood specimen.
# WTPH2YR is a for phlebotomy sample. Anyone who provided blood will have this, no matter they fasted or not.

# People partipated in the nhanes survey between 2021- 2023. 
# Demo_L records their demographic info
# HDL_L records their high density cholesterol info
# TRIGLY_L.csv records the low density cholesterol level and triglyceride levels
# TCHOL_L.csv records the total cholesterol level
# MCQ_L.csv records the whether they had angina, heart attacks, or cardiavascular diseases. 
# L indicates the year is between 2021- 2023
# All these files have different rows because only a portion of participants decide to take certain lab tests.
# The Demo_L has the most number of rows because all participants need to provide this info.


# The lab section records the lab tests

# https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Laboratory&Cycle=2021-2023


# -------inspect the data ----------------------------
#setwd("term_project")
library(dplyr)


demographic_df = read.csv("DEMO_L.csv")
# rename columns 
demographic_df <- rename(
  demographic_df,
  age     = RIDAGEYR,
  race    = RIDRETH3,
  gender  = RIAGENDR,
  marital = DMDMARTZ
)
# remove useless columns
demographic_df <- select(demographic_df, SEQN, age, race, gender, marital,SDMVPSU,SDMVSTRA,WTINT2YR
)


# ----- processing the high density info ----------
high_density_df = read.csv("HDL_L.csv")

high_density_df <- rename(
  high_density_df ,
  weight_1 = WTPH2YR ,
  high_density_level = LBDHDD
)
high_density_df <- select(high_density_df,SEQN,weight_1,high_density_level)


# ----- processing the low density and triglyceride info ----------
low_density_df = read.csv("TRIGLY_L.csv")

low_density_df <- rename(
  low_density_df,
  weight_2 = WTSAF2YR,
  triglyceride_level = LBXTLG,
  low_density_level = LBDLDLN
)

low_density_df <- select(low_density_df, SEQN, weight_2,triglyceride_level,low_density_level)

# ------processing the total --------------------
total_cholesterol_df = read.csv("TCHOL_L.csv")

total_cholesterol_df = rename(
  total_cholesterol_df,
  weight_3 = WTPH2YR,
  total_cholesterol_level = LBXTC,
)
total_cholesterol_df = select(total_cholesterol_df,SEQN,weight_3,total_cholesterol_level)

#  -----------processing the questionnaire ------------------


questionnaire_df = read.csv("MCQ_L.csv")
questionnaire_df = rename(
  questionnaire_df,
  congestive = MCQ160B,
  coronary = MCQ160C,
  angina = MCQ160D,
  heart_attack = MCQ160E
)

# questionnaire_df = select(questionnaire_df,SEQN,congestive,coronary,angina,heart_attack)
questionnaire_df = select(questionnaire_df,SEQN,coronary)

# combine dataframes by SEQN column.
# different dataframes have different rows.

result = left_join(demographic_df, high_density_df, by = "SEQN")
result =left_join(result, low_density_df, by = "SEQN")
result = left_join(result, total_cholesterol_df, by = "SEQN")
result = left_join(result, questionnaire_df, by = "SEQN")

#----- the following demonstrates that the fasting subsample is a smaller subsample than phlebotomy sample.

#  if a person has weight_2, he must has weight_1, so the following will return an empty set.
filtered_result <- subset(result, !is.na(weight_2) & is.na(weight_1)) 
# if a person has weight 1, he doesn't necessary has weight2, so the returned set shouldn't be empty.
filtered_result_2 <- subset(result, !is.na(weight_1) & is.na(weight_2)) 
filtered_result_2

# -----minor adjustments-------

result <- result %>%
  mutate(coronary = case_when(
    .data$coronary == 1 ~ 1,
    .data$coronary == 2 ~ 0,
    TRUE                ~ NA_real_
  ))



# ---- save the data -------

write.csv(result, file = "cleaned_data_with_weights.csv", row.names = FALSE)


