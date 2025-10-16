# Read CSV file into a data frame
data = read.csv("data.csv", header = FALSE)
first_col = data[[1]]
head(first_col)


# Make a histogram of hospital stay durations
hist(first_col,
     breaks = seq(min(first_col),
                  max(first_col) + 1,
                  by = 1),
     main = "Histogram of Hospital Stay Durations",
     xlab = "Days of Hospital Stay",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")


# Compute quartiles and IQR
Q1 <- quantile(first_col, 0.25, na.rm = TRUE)
Q3 <- quantile(first_col, 0.75, na.rm = TRUE)
IQR_value <- IQR(first_col, na.rm = TRUE)

# Define outlier bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- first_col[first_col < lower_bound | first_col > upper_bound]

# Print results
cat("Q1:", Q1, "\n")
cat("Q3:", Q3, "\n")
cat("IQR:", IQR_value, "\n")
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")
cat("Outliers detected:", outliers, "\n")


# Summary statistics
mean_val <- mean(first_col, na.rm = TRUE)
median_val <- median(first_col, na.rm = TRUE)
sd_val <- sd(first_col, na.rm = TRUE)
min_val <- min(first_col, na.rm = TRUE)
max_val <- max(first_col, na.rm = TRUE)

cat("\nSummary Statistics:\n")
cat("Mean:", mean_val, "\n")
cat("Median:", median_val, "\n")
cat("Standard Deviation:", sd_val, "\n")
cat("Q1:", Q1, "\n")
cat("Q3:", Q3, "\n")
cat("Minimum:", min_val, "\n")
cat("Maximum:", max_val, "\n")

# Create summary table
summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Q1", "Q3", "Minimum", "Maximum"),
  Value = c(mean_val, median_val, sd_val, Q1, Q3, min_val, max_val)
)

print(summary_table, row.names = FALSE)


# Probabilities based on normal distribution N(5, 3^2)
mean_norm <- 5
sd_norm <- 3

# (a) P(X < 10)
p_less_10 <- pnorm(10, mean = mean_norm, sd = sd_norm)

# (b) P(3 <= X <= 10)
p_between_3_10 <- pnorm(10, mean = mean_norm, sd = sd_norm) - pnorm(3, mean = mean_norm, sd = sd_norm)

cat("\nProbabilities (Normal distribution N(5,3^2)):\n")
cat("Percentage of patients with stays < 10 days:", round(p_less_10 * 100, 2), "%\n")
cat("Percentage of patients with stays between 3 and 10 days:", round(p_between_3_10 * 100, 2), "%\n")


# last question


mu = 5
sigma = 3
n = 35

sigma_clt = 3 / sqrt(35)
z_score = (6 - mu) / sigma_clt

p = pnorm(z_score,lower.tail = FALSE)
cat("The probability of observing a sample with mean > 6 is ", p)

