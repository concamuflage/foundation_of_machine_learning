# suppose our model made the predictions about probabilites for 10 observations
# we can plot a histogram like the following to show the confidence of our model. 
# this histogram is the overall learned probability distribution

# Interpretation of the histogram:
# --------------------------------
# Most bars are on the left (near 0–0.3):
#   → Most samples were predicted to have a low probability of the positive outcome (e.g., default).
#
# A few bars are on the right (0.7–1.0):
#   → These are cases where the model is confidently predicting the positive class (Y = 1).
#
# The middle range (0.4–0.6) has very few samples:
#   → The model rarely produces uncertain predictions around 50%.
#
# In plain English:
#   → The model is quite confident overall — it tends to output probabilities close to 0 or 1,
#     with few predictions in the uncertain middle range.


# Simulated predicted probabilities from a logistic regression model
p_i <- c(0.05, 0.70, 0.30, 0.10, 0.15, 0.80, 0.55, 0.25, 0.40, 0.95)

# Create a histogram
hist(
  p_i,
  breaks = seq(0, 1, by = 0.1),   # bins from 0 to 1
  col = "lightblue",
  main = "Distribution of Predicted Probabilities",
  xlab = "Predicted Probability (p_i)",
  ylab = "Number of Samples",
  border = "white"
)

# Add a rug (small tick marks for each point)
rug(p_i, col = "darkblue", lwd = 2)