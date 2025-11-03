
# for one way anova pairwise test
# population variances for each group is assumed to be equal.


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
pooled_variance = 43.2 # this is equal to MSW in the anova table

#-----------------------------------------------
# Assume the unknown population variances of the two groups are the same.
# We use pooled variance.
#-----------------------------------------------


# calculate the t_statistic

  
t_statistic = (group_one_mean - group_two_mean) / sqrt(pooled_variance*(1/group_one_count+1/group_two_count))
t_statistic

# two sided


# from notes when in the context of pairwise comparison 

df_pooled = total_number_of_observations - total_number_of_groups

cat("two sided test\n")
t_critical = qt(1-alpha/2,df = df_pooled )
t_critical 
p_value = 2*(1-pt(abs(t_statistic),df = df_pooled)) # result 0.000349153
p_value 

if (abs(t_statistic) > abs(t_critical) ){
  cat("reject\n")
} else {
  cat("fail to reject \n")
}
if (p_value < alpha){
  cat("reject\n")
} else {
  cat("fail to reject\n")
}


# using buildin method for pairwise comparision

pairwise.t.test(data$SBP,data$group,p.adjust = "none",pool.sd = TRUE) # result 0.01


# from notes when comparing two samples; use this one if you want the result the same as pairwise function. 

df_welch <- (group_one_sd^2/group_one_count + group_two_sd^2/group_two_count)^2 /
  ((group_one_sd^2/group_one_count)^2/(group_one_count - 1) +
     (group_two_sd^2/group_two_count)^2/(group_two_count - 1))



