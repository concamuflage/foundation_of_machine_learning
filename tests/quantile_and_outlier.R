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