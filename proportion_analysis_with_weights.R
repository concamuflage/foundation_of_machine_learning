weighted_data = read.csv("cleaned_data_with_weights.csv")
head(weighted_data)



library(survey)

# Create survey design using interview weight
design_age <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTINT2YR,
  nest = TRUE,
  data = weighted_data
)

# Weighted histogram
svyhist(~age,
        design = design_age,
        breaks = seq(
          floor(min(weighted_data$age, na.rm = TRUE)),
          ceiling(max(weighted_data$age, na.rm = TRUE)),
          by = 5
        ),
        main = "Weighted Age Distribution (5-year buckets)",
        xlab = "Age",
        col = "lightblue")

# ---------coronary prevalence --------------

# Labels
coronary_labels <- c("1" = "Yes", "0" = "No")

# Extract variables
cor <- weighted_data$coronary
wts <- weighted_data$WTINT2YR

# ---- Weighted counts ----
weighted_counts <- tapply(wts, cor, sum, na.rm = TRUE)

# Replace names with labels
names(weighted_counts) <- coronary_labels[names(weighted_counts)]

# ---- Percentages ----
pct <- round(weighted_counts / sum(weighted_counts) * 100)

# Labels for pie chart
coronary_lbl <- paste0(names(weighted_counts), " (", pct, "%)")

# Colors
colors <- rainbow(length(weighted_counts))

# ---- Draw weighted pie chart ----
pie(
  weighted_counts,
  labels = coronary_lbl,
  main = "Weighted Coronary Disease Distribution",
  col = colors
)