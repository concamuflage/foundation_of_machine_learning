data = read.csv("cleaned_data.csv")
head(data)

# pie chart 

# ----------histogram for age ----------------
hist(
  data$age,
  breaks = seq(floor(min(data$age, na.rm = TRUE)),
               ceiling(max(data$age, na.rm = TRUE)),
               by = 5),
  main = "Age Distribution (5-year buckets)",
  xlab = "Age",
  col = "lightblue",
  border = "black"
)


# ----------race pie chart----------------
race_labels <- c(
  "1" = "Mexican American",
  "2" = "Other Hispanic",
  "3" = "Non-Hispanic White",
  "4" = "Non-Hispanic Black",
  "6" = "Non-Hispanic Asian",
  "7" = "Other Race - Including Multi-Racial"
)

race_counts <- table(race_labels[as.character(data$race)])
colors <- rainbow(length(race_counts))

# compute percentages
pct <- round(race_counts / sum(race_counts) * 100)

# create label strings: "Label (xx%)"
pie_labels <- paste0(names(race_counts), " (", pct, "%)")

pie(
  race_counts,
  labels = pie_labels,
  main = "Race Distribution",
  col = colors
)

# ----------gender pie chart----------------
# Gender labels
gender_labels <- c(
  "1" = "Male",
  "2" = "Female"
)

# Count
gender_counts <- table(gender_labels[as.character(data$gender)])

# Colors
colors <- rainbow(length(gender_counts))

# Percentages
gender_pct <- round(gender_counts / sum(gender_counts) * 100)

# Labels with percentages
gender_lbl <- paste0(names(gender_counts), " (", gender_pct, "%)")

# Draw pie
pie(
  gender_counts,
  labels = gender_lbl,
  main = "Gender Distribution",
  col = colors
)

# ----------marital pie chart----------------

# Marital labels
marital_labels <- c(
  "1"  = "Married/Living with partner",
  "2"  = "Widowed/Divorced/Separated",
  "3"  = "Never married"
)

# Count
marital_counts <- table(marital_labels[as.character(data$marital)])

# Colors
colors <- rainbow(length(marital_counts))

# Percentages
marital_pct <- round(marital_counts / sum(marital_counts) * 100)

# Labels with percentages
marital_lbl <- paste0(names(marital_counts), " (", marital_pct, "%)")

# Draw pie
pie(
  marital_counts,
  labels = marital_lbl,
  main = "Marital Status Distribution",
  col = colors
)

# --------pie chart for coronary disease----------

# Coronary labels (valid responses only)
coronary_labels <- c(
  "1" = "Yes",
  "0" = "No" # original value 2
)

# Count
coronary_counts <- table(coronary_labels[as.character(data$coronary)])

# Colors
colors <- rainbow(length(coronary_counts))

# Percentages
pct <- round(coronary_counts / sum(coronary_counts) * 100)

# Combined labels: "Yes (xx%)"
coronary_lbl <- paste0(names(coronary_counts), " (", pct, "%)")

# Draw pie chart
pie(
  coronary_counts,
  labels = coronary_lbl,
  main = "Coronary Distribution",
  col = colors
)


