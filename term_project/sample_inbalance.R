data = read.csv("cleaned_data.csv")
head(data)

# ------------- scatter plot with categorical point marker 1 -------------
# Extract variables
x <- data$low_density_level    # LDL
y <- data$high_density_level         # HDL
cor <- data$coronary               # 1 = Yes, 2 = No

# Empty plot first
plot(
  x, y,
  type = "n",
  xlab = "Low-Density Cholesterol (LDL)",
  ylab = "High-Density Cholesterol (HDL)",
  main = "LDL vs HDL with Coronary Status"
)

# Add green circles for No coronary disease (2)
points(
  x[cor == 2],
  y[cor == 2],
  col = "green",
  pch = 19
)

# Add red crosses for Yes coronary disease (1)
points(
  x[cor == 1],
  y[cor == 1],
  col = "red",
  pch = 4,
  cex = 1.2,
  lwd = 2
)

legend(
  "topright",
  legend = c("Coronary = Yes", "Coronary = No"),
  col = c("red", "green"),
  pch = c(4, 19),
  pt.cex = 1.2,
  bty = "n"
)

# ------------- scatter plot with categorical point marker 2 -------------

# Extract variables
x <- data$total_cholesterol_level     # Total cholesterol
y <- data$triglyceride                # Triglycerides
cor <- data$coronary                  # 1 = Yes, 2 = No

# Empty plot first
plot(
  x, y,
  type = "n",
  xlab = "Total Cholesterol Level",
  ylab = "Triglyceride Level",
  main = "Total Cholesterol vs Triglycerides with Coronary Status"
)

# Add green circles for No coronary disease (2)
points(
  x[cor == 2],
  y[cor == 2],
  col = "green",
  pch = 19
)

# Add red crosses for Yes coronary disease (1)
points(
  x[cor == 1],
  y[cor == 1],
  col = "red",
  pch = 4,
  cex = 1.2,
  lwd = 2
)

# Legend
legend(
  "topright",
  legend = c("Coronary = Yes", "Coronary = No"),
  col = c("red", "green"),
  pch = c(4, 19),
  pt.cex = 1.2,
  bty = "n"
)
