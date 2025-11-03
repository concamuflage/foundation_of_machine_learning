source("compare.r")
# for one way anova pairwise test
# without assuming equal population variances for the two groups.


# load the data

data = read.csv("data/smoking_SBP.csv")
head(data)
summary(data)

# hypothesis
# h0: group_one and group_two have the same means
# h1: their means are same

alpha = 0.05
total_number_of_observations = nrow(data) 
total_number_of_groups = 4


tapply(data$SBP,data$group,mean)  # get the mean for each group
tapply(data$SBP,data$group,sd)    # get the variance for each group
tapply(data$SBP,data$group,length) # get the number of observations for each group
tapply(data$SBP,data$group,sum) 

aggregate(data$SBP ~ data$group,data = data,mean)


#-----------------------------------------------
# must fill in manually from the above table
group_one_mean = 124.2500  
group_two_mean = 111.8333 
group_one_sd = 3.5
group_two_sd = 5.04
group_one_count = 4
group_two_count = 6

# -----------------------------------------------------
# calculate manually


# calculate the t_statistic

df_welch = (group_one_sd^2/group_one_count + group_two_sd^2/group_two_count)^2 /
  ((group_one_sd^2/group_one_count)^2/(group_one_count - 1) +
     (group_two_sd^2/group_two_count)^2/(group_two_count - 1))

t_statistic = (group_one_mean - group_two_mean) / sqrt(group_one_sd^2/group_one_count+group_two_sd^2/group_two_count)
t_statistic


# from notes when in the context of pairwise comparison 

df_pooled = total_number_of_observations - total_number_of_groups

cat("two sided test\n")
t_critical = qt(1-alpha/2,df = df_welch )
t_critical 
p_value = 2*(1-pt(abs(t_statistic),df = df_welch)) # result 0.000349153
p_value 

compareTwoSided(t_statistic,t_critical,p_value,alpha)

# -----------------------------------------------------
# using the built in method
pairwise.t.test(data$SBP,data$group,p.adjust = "none",pool.sd = FALSE) # pool.sd must be set to FALSE!


