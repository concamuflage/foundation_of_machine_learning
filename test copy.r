# Load the CSV file  
students <- read.csv("students.csv")

# Take a quick look at the data  
head(students)
summary(students)

# Build a linear model  
# Replace 'score' with the dependent variable (y)
# and 'study_hours' with the predictor variable (x)
model <- lm(score ~ hours, data = students)

# Show model summary  
print(model)
confint(model)


