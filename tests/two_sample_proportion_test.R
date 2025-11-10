# setwd("/Users/Mark_1/foundation_of_machine_learning/tests")
source("compare.R")

# given two sample proportions, test if they are equal at at certain alpha level

# ----------edit area --------------

sample_size_one = 320
sample_size_two = 360
sample_proportion_one = 76/320
sample_proportion_two = 94/360

alpha = 0.05

# ----------edit area --------------

# H0:population_proportion_one = population_proportion_two

total_positive = sample_proportion_one*sample_size_one + sample_proportion_two*sample_size_two
pooled_sample_proportion = total_positive/(sample_size_one+sample_size_two)
standard_error = sqrt(pooled_sample_proportion*(1 - pooled_sample_proportion)*(1/sample_size_one+1/sample_size_two))

z_statistic = (sample_proportion_one-sample_proportion_two)/standard_error
z_statistic

# -----------testing code --------------------------
# two sided
cat("two sided test\n")

z_critical= qnorm(1-alpha/2)
z_critical
p_value = 2*(1-pnorm(abs(z_statistic)))
p_value 
compareTwoSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)


# right sided
cat("right sided test\n")
z_critical= qnorm(1-alpha)
z_critical
p_value = 1 - pnorm(z_statistic)
p_value
compareRightSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)



# left sided
cat("left sided test\n")
z_critical= qnorm(alpha)
z_critical
p_value = pnorm(z_statistic)
p_value 
compareLeftSided(z_statistic,z_critical)
comparePvalueAlpha(p_value,alpha)
