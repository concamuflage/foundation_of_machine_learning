# -----------------------------------------------
# Example: Partitioning SSreg in a two-parameter MLR
# -----------------------------------------------

# How is the SSreg for X2 is calculated?
# Fit a model with X1 and calculate residual ss
# Fit another model with both X1 and X2, calculate the new residual ss.
# the reduction in residual ss is the variance explained by X2 or the regressonal ss for X2.



# Step 1: Create dataset (14 observations total)
data <- data.frame(
  X1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
  X2 = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11, 14, 13),
  Y  = c(2, 3, 6, 7, 9, 11, 13, 14, 17, 18, 21, 22, 25, 26)
)
# Step 2: Fit models
m1 <- lm(Y ~ X1, data = data)        # model with X1 only
m2 <- lm(Y ~ X1 + X2, data = data)   # full model with X1 and X2

# Step 3: Compare models using ANOVA
anova(m1, m2)  # extra variation explained by X2 after X1

#        Res.Df  RSS  Df      Sum of Sq          F           Pr(>F)
# 1      2       0.8                                                           for first model
# 2      1       0.0  1       descrease in RSS  1.4013e+31   < 2.2e-16 ***      for second model


# RSS means residual sum of squares; 
# a decrease in RSS in model 2 means more variance is explained by model 2 = extra regressional SS for model2.
# this extra regressional SS in our new model is regressional ss for component X2 in the ANOVA table.

# 1.4013e+31 the f_statistic for testing if the decrease is significant. 
# the p_value for the f_test
# the decrease in RSS is the same SSreg for X2 given in ANOVA table for 


# Step 4: View total SS decomposition for the full model
anova(m2)
