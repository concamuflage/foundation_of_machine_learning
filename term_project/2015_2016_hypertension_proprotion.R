# In this script, the importance of using weight is showed.
# 
# this is to replicate the second sample of using weights in this link.
# https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx

# data source
# https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&Cycle=2015-2016

# ---------------without weights ------------------------------
library(haven)
bpx <- read_xpt("BPX_2015_2016.XPT")   # exam
demo = read_xpt("DEMO_2015_2016.XPT") # demographics
bpq = read_xpt("BPQ_2015_2016.XPT")  # questionaire
sort(names(result))

# keep only participants who are in BOTH files
nrow(demo)
nrow(bpx)
result <- merge(demo, bpx, by = "SEQN", all = FALSE) # now result only include individuals who did Mec/ in bpx.


nrow(result)

no_bpx <- result %>%
  filter(
    is.na(BPXSY1), is.na(BPXSY2), is.na(BPXSY3), is.na(BPXSY4),
    is.na(BPXDI1), is.na(BPXDI2), is.na(BPXDI3), is.na(BPXDI4)
  )

nrow(no_bpx)  # among all people who did MEC, this number of people didn't do any blood pressure

# join with bpq 
result = left_join(result, bpq, by = "SEQN")
nrow(result)

taking_medication = subset(result, result$BPQ050A == 1)


library(dplyr)

taking_medication <- result %>%
  filter(
    BPQ050A == 1,
    is.na(BPXSY1), is.na(BPXSY2), is.na(BPXSY3), is.na(BPXSY4),
    is.na(BPXDI1), is.na(BPXDI2), is.na(BPXDI3), is.na(BPXDI4)
  )
nrow(taking_medication)

head(result)
dim(result)
names(result)

colSums(is.na(result))

# calculate average of blood pressure 

result$average_systolic = rowMeans(
  result[, c("BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4")],
  na.rm = TRUE
)

result$average_diastolic = rowMeans(
  result[, c("BPXDI1", "BPXDI2", "BPXDI3", "BPXDI4")],
  na.rm = TRUE
)

# --------with weights----------------------

# Hypertension indicator (same as CDC tutorial example)
result$hypertension <- with(result,
                            (average_systolic >= 140 |
                               average_diastolic >= 90 |
                               BPQ050A == 1) &
                              RIDAGEYR >= 18
)

# Create survey design object
design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  nest = TRUE,
  data = result
)

# Weighted prevalence
svymean(~hypertension, design, na.rm = TRUE)  # the result from the study 32.1, which is far smaller than my result, but I couldn't figure out why.






