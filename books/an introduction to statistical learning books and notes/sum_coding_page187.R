# ============================
# Example Data
# ============================
Y <- c(9, 10, 11, 10, 10,    # Group A
       14, 15, 13, 14, 14)   # Group B

Group <- factor(rep(c("A", "B"), each = 5))
data <- data.frame(Y, Group)

# ============================
# Set Sum Coding (contr.sum)
# ============================
contrasts(data$Group) <- contr.sum(2)
contrasts(data$Group)
# This prints the 2-level contrast matrix:
#     [,1]
# A     1
# B    -1

# ============================
# Fit the linear model
# ============================
model_sum <- lm(Y ~ Group, data = data)
summary(model_sum)

# ============================
# Predicted means for each level
# ============================
coef(model_sum)

grand_mean <- coef(model_sum)[1]              # intercept
group_contrast <- coef(model_sum)[2]          # deviation

mean_A <- grand_mean + group_contrast
mean_B <- grand_mean - group_contrast

mean_A
mean_B