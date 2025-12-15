# setwd("/Users/Mark_1/foundation_of_machine_learning/tests")
source("compare.R")

# given two sample proportions, test if they are equal at at certain alpha level

# --------------edit area if you are given a dataset-------------------

#number_of_males = nrow(subset(data, sex == 1 ))
#number_of_females = nrow(subset(data, sex == 2 ))
#number_of_positive_in_males = nrow(subset(data, sex == 1 & temp_level == 1))
# number_of_positive_in_females = nrow(subset(data, sex == 2 & temp_level == 1))


# ----------edit area if you are given the numbers --------------

sample_size_one = 320
sample_size_two = 360
positive_size_one = 76
positive_size_two = 94
sample_proportion_one = positive_size_one/sample_size_one
sample_proportion_two = positive_size_two/sample_size_two
alpha = 0.05
# ----------edit area --------------


# H0:population_proportion_one = population_proportion_two

# automatic. check the p-value. If it is smaller than alpha, rejct. (use this approach in exam)
# the p_values are the same as the manual tests when correction is FALSE.
# two sided
prop.test(c(positive_size_one,positive_size_two),c(sample_size_one,sample_size_two),correct = FALSE,alternative = "two.sided")
# right.sided
prop.test(c(positive_size_one,positive_size_two),c(sample_size_one,sample_size_two),correct = FALSE,alternative = "greater") 
# left.sided
prop.test(c(positive_size_one,positive_size_two),c(sample_size_one,sample_size_two),correct = FALSE,alternative = "less")

# manual test


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
