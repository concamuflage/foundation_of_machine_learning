# ---------------- ANCOVA Example ----------------
# Relationship between SBP (blood pressure), smoking status, and age
# Result:

# The parallels are fit by observations in each group.
# The distance between them at any point is 5, which is the same as the real difference in the true model.
# In the printout, the difference between the two emmeans are also 5. 
# Verified that the following is the correct way to calculate emmeans.

# emmeans_smoker = 108.83+5.45*1+0.6218*average_age
# emmeans_non_smoker = 108.8331+5.4563*0+0.6218*average_age


# Step 1: Simulate data
set.seed(123)
n <- 40
age <- runif(n, 30, 60)                       # Covariate
group <- rep(c("NonSmoker", "Smoker"), each = n/2)  # Categorical variable

# True model: SBP increases with age, smokers have +5 offset
SBP <- 110 + 0.6 * age + ifelse(group == "Smoker", 5, 0) + rnorm(n, 0, 3)

data <- data.frame(SBP, age, group)

# Step 2: Fit ANCOVA model
model <- lm(SBP ~ group + age, data = data)
summary(model)

# Step 3: Check adjusted means (least squares means)
library(emmeans)
emmeans(model, specs = "group")

# Step 4: Visualization
library(ggplot2)

ggplot(data, aes(x = age, y = SBP, color = group)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "ANCOVA: Adjusted Mean Difference Between Groups",
    subtitle = "Parallel lines indicate same age effect; vertical gap = adjusted mean difference",
    x = "Age",
    y = "Systolic Blood Pressure (SBP)"
  ) +
  theme_minimal()
# -------------------------------------------------