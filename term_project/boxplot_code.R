data = read.csv("cleaned_data.csv")
head(data)


# Make coronary a factor with labels
coronary_factor <- factor(
  data$coronary,
  levels = c(1, 2),
  labels = c("Yes", "No")
)

# Boxplot
boxplot(
  triglyceride_level ~ coronary_factor,
  data = data,
  xlab = "Coronary Disease",
  ylab = "Triglyceride Level",
  main = "Triglyceride Levels by Coronary Status",
  col = c("red", "green")
)

boxplot(
  high_density_level ~ coronary_factor,
  data = data,
  xlab = "Coronary Disease",
  ylab = "HDL (High-Density Cholesterol)",
  main = "HDL Levels by Coronary Status",
  col = c("red", "green")
)

boxplot(
  low_density_level ~ coronary_factor,
  data = data,
  xlab = "Coronary Disease",
  ylab = "LDL (Low-Density Cholesterol)",
  main = "LDL Levels by Coronary Status",
  col = c("red", "green")
)

boxplot(
  total_cholesterol_level ~ coronary_factor,
  data = data,
  xlab = "Coronary Disease",
  ylab = "Total Cholesterol",
  main = "Total Cholesterol by Coronary Status",
  col = c("red", "green")
)