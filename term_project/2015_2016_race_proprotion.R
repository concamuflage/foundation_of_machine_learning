# First, we try to estimate the race proprotion in the US population without using weights.
# Second, we estimate the same parameter with weight, which is closer to the true populations. 
# We choose this weight because WTINT2YR is interview weight. 
# This is to replicate the histograms in the following link

# https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx




library(haven)
demo <- read_xpt("DEMO_2015_2016.XPT")   # demographics


table(demo$RIDRETH3)

demo$race3 <- dplyr::case_when(
  demo$RIDRETH3 %in% c(1, 2) ~ "Hispanic",
  demo$RIDRETH3 == 4 ~ "Non-Hispanic Black",
  demo$RIDRETH3 == 6 ~ "Non-Hispanic Asian",
  demo$RIDRETH3 %in% c(3, 7) ~ "Non-Hispanic White & Other",
  TRUE ~ NA_character_
)




demo$race3 <- factor(
  demo$race3,
  levels = c("Hispanic",
             "Non-Hispanic Black",
             "Non-Hispanic Asian",
             "Non-Hispanic White & Other")
)


# to calculate manually.
sum(demo$race3 == "Hispanic", na.rm = TRUE)/nrow(demo)



barplot(
  table(demo$race3),
  main = "Distribution of Race/Ethnicity",
  xlab = "Race Group",
  ylab = "Count"
)

race_counts <- table(demo$race3)
race_percent <- 100 * race_counts / sum(race_counts)

race_percent

bp <- barplot(
  race_percent,
  ylim = c(0, max(race_percent) + 5),  # extra space for labels
  main = "Percentage of Race/Ethnicity Groups",
  ylab = "Percentage (%)",
  xlab = "Race Group"
)

# Add text labels above each bar
text(
  x = bp,
  y = race_percent,
  labels = paste0(round(race_percent, 1), "%"),
  pos = 3,      # top (above bar)
  cex = 0.9
)


# ---------to generate the weighted histogram ------------


# weighted sum per race
weighted_counts <- tapply(demo$WTINT2YR, demo$race3, sum, na.rm = TRUE)

# to calculate weighted percentage for Hispanic manually 
weighted_counts_hispanic_manual = sum(subset(demo, demo$race3 == "Hispanic")$WTINT2YR)
weighted_counts_total_manual = sum(demo$WTINT2YR)
weighted_percent_for_hispanic = weighted_counts_hispanic_manual/weighted_counts_total_manual

# convert to percentage
weighted_percent <- 100 * weighted_counts / sum(weighted_counts)
weighted_percent

bp <- barplot(
  weighted_percent,
  ylim = c(0, max(weighted_percent) + 5),
  main = "Weighted Percentage of Race/Ethnicity Groups",
  ylab = "Weighted Percentage (%)",
  xlab = "Race Group"
)

# add % labels
text(
  x = bp,
  y = weighted_percent,
  labels = paste0(round(weighted_percent, 1), "%"),
  pos = 3,
  cex = 0.9
)
