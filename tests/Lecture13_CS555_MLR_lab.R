###############
# CS 555
# MLR lab


############################################################
# 0. Setup and Data
############################################################

data(mtcars)

df <- mtcars
df$cyl <- factor(df$cyl)
df$am  <- factor(df$am, labels = c("auto", "manual"))

str(df)
summary(df$mpg)



############################################################
# 1. Simple Linear Regression
############################################################

slr_model <- lm(mpg ~ wt, data = df)
summary(slr_model)

plot(df$wt, df$mpg,
     xlab = "Weight (1000 lbs)",
     ylab = "Miles per gallon",
     main = "Simple Linear Regression: mpg ~ wt")
abline(slr_model, col = "red", lwd = 2)



############################################################
# 2. Multiple Linear Regression
############################################################

mlr_model <- lm(mpg ~ wt + hp + disp + cyl, data = df)
summary(mlr_model)

# Check differences with SLR and MLR
smry_slr = summary(slr_model)
smry_mlr = summary(mlr_model)

smry_slr$r.squared
smry_mlr$r.squared

smry_slr$adj.r.squared
smry_mlr$adj.r.squared



############################################################
# 3. Model backward Selection
############################################################

full_model <- lm(mpg ~ wt + hp + disp + drat + qsec + cyl + am, data = df)
summary(full_model)

#remove cyl
full_model <- lm(mpg ~ wt + hp + disp + drat + qsec + am, data = df)
summary(full_model)

# remove drat
full_model <- lm(mpg ~ wt + hp + disp + qsec + am, data = df)
summary(full_model)

# remove disp
full_model <- lm(mpg ~ wt + hp + qsec + am, data = df)
summary(full_model)

# remove hp
full_model <- lm(mpg ~ wt + qsec + am, data = df)
summary(full_model)


############################################################
# 3.2.(EXTRA) Model Selection (AIC-based Stepwise)
############################################################

full_model <- lm(mpg ~ wt + hp + disp + drat + qsec + cyl + am, data = df)

step_model <- step(full_model, direction = "both", trace = FALSE)
summary(step_model)

formula(step_model)



############################################################
# 4. Ridge Regression (L2 Regularization)
############################################################

# install.packages("glmnet")
library(glmnet)

X <- model.matrix(mpg ~ wt + hp + disp + drat + qsec + cyl + am, data = df)[, -1]
y <- df$mpg

set.seed(123)
ridge_cv <- cv.glmnet(X, y, alpha = 0)
plot(ridge_cv)

ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_min

ridge_fit <- glmnet(X, y, alpha = 0, lambda = ridge_lambda_min)
ridge_coefs <- coef(ridge_fit)
ridge_coefs



############################################################
# 5. Lasso Regression (L1 Regularization)
############################################################

set.seed(123)
lasso_cv <- cv.glmnet(X, y, alpha = 1)
plot(lasso_cv)

lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_min

lasso_fit <- glmnet(X, y, alpha = 1, lambda = lasso_lambda_min)
lasso_coefs <- coef(lasso_fit)
lasso_coefs

nonzero_idx <- which(lasso_coefs != 0)
selected_vars <- rownames(lasso_coefs)[nonzero_idx]
selected_vars



############################################################
# 6. Interaction in Multiple Linear Regression
############################################################

mlr_int_model <- lm(mpg ~ wt + am + wt*am + hp + cyl, data = df)
summary(mlr_int_model)



############################################################
# 7. Polynomial Regression
############################################################

poly_model <- lm(mpg ~ wt + I(wt^2) + hp + cyl, data = df)
summary(poly_model)

summary(mlr_model)$coefficients["wt", ]
summary(poly_model)$coefficients[c("wt", "I(wt^2)"), ]

plot(df$wt, df$mpg,
     xlab = "Weight (1000 lbs)", ylab = "MPG",
     main = "Polynomial Regression: mpg ~ wt + I(wt^2)")
wt_seq <- seq(min(df$wt), max(df$wt), length.out = 100)

pred_poly <- predict(poly_model, newdata = data.frame(
  wt = wt_seq,
  hp = mean(df$hp),
  cyl = factor("6", levels = levels(df$cyl))
))
lines(wt_seq, pred_poly, col = "blue", lwd = 2)



############################################################
# 8. Regularization on Polynomial Regression
############################################################

poly_formula <- mpg ~ poly(wt, 3) + poly(hp, 2) + cyl + am

X_poly <- model.matrix(poly_formula, data = df)[, -1]
y_poly <- df$mpg

# (a) Ridge on polynomial
set.seed(123)
ridge_poly_cv <- cv.glmnet(X_poly, y_poly, alpha = 0)
ridge_poly_lambda <- ridge_poly_cv$lambda.min

ridge_poly_fit <- glmnet(X_poly, y_poly, alpha = 0, lambda = ridge_poly_lambda)
ridge_poly_coefs <- coef(ridge_poly_fit)
ridge_poly_coefs

# (b) Lasso on polynomial
set.seed(123)
lasso_poly_cv <- cv.glmnet(X_poly, y_poly, alpha = 1)
lasso_poly_lambda <- lasso_poly_cv$lambda.min

lasso_poly_fit <- glmnet(X_poly, y_poly, alpha = 1, lambda = lasso_poly_lambda)
lasso_poly_coefs <- coef(lasso_poly_fit)
lasso_poly_coefs

# Polynomial terms that survive lasso:
nonzero_poly <- rownames(lasso_poly_coefs)[which(lasso_poly_coefs != 0)]
nonzero_poly

