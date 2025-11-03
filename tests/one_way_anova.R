# for one way anova

# load the data

data = read.csv("data/smoking_SBP.csv")
head(data)

# hypothesis
# h0: all means are equal
# h1: at least two means are different.

# 
alpha = 0.05
total_number_of_observations = nrow(data) 
total_number_of_groups = 4

# Global F_test

is.factor(data$group) # check if group column is a factor

model = aov(data$SBP~ data$group,data = data)
summary(model)

# notes about the summary table

#              Df   Sum Sq        Mean Sq      F value    Pr(>F)    
#data$group     3   (SSB)         (MSB)   21.49    1.1e-05 *** (Between Group)
# Residuals     15   (SSW)        (MSW)                        (Within Group)


f_statistic = summary(model)[[1]]$`F value`[1]
df_within = total_number_of_observations -total_number_of_groups - 1
df_between = total_number_of_groups - 1
f_critical = qf(1-alpha,df_between,df_within)


# two sided
if (f_statistic > f_critical){
  cat("reject \n")
} else {
  cat("fail to reject \n")
}

# Pairwise Test
# the pairwise test if the H0 is rejected








