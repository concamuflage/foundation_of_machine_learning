# # Create a sample data frame
# data <- data.frame(
#   Name = c("Alice","Bob","Charlie","David","Eve"),
#   Education = factor(c("High School","Bachelor","Bachelor","Master","PhD"))
# )

# # Show proportions directly
# proportions <- summary(data$Education) / nrow(data)

# print(summary(data$Education))  # counts
# print(proportions)              # proportions

earning <- c(35, 40, 145, 33, 30, 42, 32, 32, 25)

summary(earning)
boxplot(earning)


