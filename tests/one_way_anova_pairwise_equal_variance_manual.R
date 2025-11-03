source("compare.R")

# for one way anova pairwise test
# population variances for each group is assumed to be equal.

# hypothesis
# h0: group_one and group_two have the same means
# h1: their means are same

# ----------------- change section --------
data = read.csv("data/golf_distance.csv")
alpha = 0.05
total_number_of_observations = nrow(data) 
total_number_of_groups = length(unique(data$brand))

tapply(data$distance,data$brand,mean)  # get the mean for each group
tapply(data$distance,data$brand,sd)    # get the variance for each group
tapply(data$distance,data$brand,length) # get the number of observations for each group
tapply(data$distance,data$brand,sum) 

aggregate(data$distance ~ data$brand,data = data,mean)
# ----------------- change section --------------------

total_number_of_paired_test = total_number_of_groups * (total_number_of_groups-1) /2
alpha = alpha / total_number_of_paired_test # adjust the alpha for each test

#------------------must fill in manually from the above table -----------------------------

group_one_mean = 285 
group_two_mean = 260 
group_one_sd = 2.236068
group_two_sd = 2.236068
group_one_count = 5
group_two_count = 5
pooled_variance = 5 # this is equal to MSW in the anova table

#-----------------------------------------------
# Assume the unknown population variances of the two groups are the same.
# We use pooled variance.
#-----------------------------------------------


# calculate the t_statistic

  
t_statistic = (group_one_mean - group_two_mean) / sqrt(pooled_variance*(1/group_one_count+1/group_two_count))
t_statistic

df_pooled = total_number_of_observations - total_number_of_groups

cat("two sided test\n")
t_critical = qt(1-alpha/2,df = df_pooled )
t_critical 
p_value = 2*(1-pt(abs(t_statistic),df = df_pooled)) 
p_value 

compareTwoSided(t_statistic,t_critical,p_value,alpha)

# ---------------automated test ------------------------------------------------
# Unjusted 
pairwise.t.test(data$distance,data$brand,p.adjust = "none",pool.sd = TRUE) 

# Bonferroni procedure
pairwise.t.test(data$distance,data$brand,p.adjust = "bonferroni",pool.sd = TRUE) 
# Tukey procedure
model = aov(data$distance ~ data$brand,data = data)
TukeyHSD(model)



