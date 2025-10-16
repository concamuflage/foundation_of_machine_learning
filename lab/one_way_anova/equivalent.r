# You’ll see both give the same sums of squares — 
# showing that one-way ANOVA is just a special case of linear regression, 
# where the predictor is categorical.


# Example with 3 groups
group <- factor(rep(c("A", "B", "C"), each = 5))
score <- c(rnorm(5, 70, 5), rnorm(5, 75, 5), rnorm(5, 80, 5))

anova_model <- aov(score ~ group)
summary(anova_model)

# Compare with regression form
lm_model <- lm(score ~ group)
anova(lm_model)