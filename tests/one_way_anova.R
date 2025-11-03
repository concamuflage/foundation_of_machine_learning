source("compare.R")

# for one way anova

# load the data

# hypothesis
# h0: all means are equal
# h1: at least two means are different.

# ------------------- change variables here ------------------------
data = read.csv("data/golf_distance.csv")
alpha = 0.05
total_number_of_groups = length(unique(data$brand))
model = aov(data$distance ~ data$brand,data = data)
# ------------------------------------------------------------------

total_number_of_observations = nrow(data)
summary(model)

# notes about the summary table

#              Df   Sum Sq        Mean Sq      F value    Pr(>F)    
#data$group     3   (SSB)         (MSB)   21.49    1.1e-05 *** (Between Group)
# Residuals     15   (SSW)        (MSW)                        (Within Group)

# --------------Global F_test ---------------------------------------------------------------

f_statistic = summary(model)[[1]]$`F value`[1]
df_within = total_number_of_observations -total_number_of_groups - 1
df_between = total_number_of_groups - 1
f_critical = qf(1-alpha,df_between,df_within)
p_value = 1 - pf(f_statistic,df_between,df_within)
p_value  


# right sided
compareRightSided(f_statistic,f_critical,p_value,alpha)

# Pairwise Test
# the pairwise test if the H0 is rejected








