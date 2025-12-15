# H0: mu1- mu2 != 0 (two sided)
# H1: mu1 - mu2 = 0

# -------------edit area ---------------------
df <- read.csv("data/calories.csv")
participants <- df$participants
participants <- na.omit(participants)
non_participants <- df$non_participants
non_participants <- na.omit(non_participants)
mean_participants = mean(participants)
mean_non_participants = mean(non_participants)
sample_sd_participants = sd(participants)
sample_sd_non_participants = sd(non_participants)
num_participants = length(participants)
num_non_participants = length(non_participants)
alpha = 0.05 # not Confidence Level.

# ------------edit area -------------------

# ----manual test ----

standard_error = sqrt(sample_sd_participants^2/num_participants +sample_sd_non_participants^2/num_non_participants)
t_statistic =  (mean_participants - mean_non_participants) / standard_error
t_statistic

welch_df =
  (sample_sd_participants^2 / num_participants +
     sample_sd_non_participants^2 / num_non_participants)^2 /
  (
    (sample_sd_participants^2 / num_participants)^2 / (num_participants - 1) +
      (sample_sd_non_participants^2 / num_non_participants)^2 / (num_non_participants - 1)
  )

welch_df


# two sided
cat("two sided test\n")

t_critical = qt(1-alpha/2,welch_df)
t_critical 
p_value = 2*(1-pt(abs(t_statistic),welch_df))
p_value 

compareTwoSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)

# right sided ( H1: mu > mu0) NOT H0
cat("right sided test\n")
t_critical = qt(1-alpha,welch_df)
t_critical 
p_value = 1 - pt(t_statistic,welch_df)
p_value

compareRightSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)


# left sided ( H1: mu < mu0) NOT H0
cat("left sided test\n")
t_critical = qt(alpha,welch_df)
t_critical 
p_value = pt(t_statistic,welch_df)
p_value 

compareLeftSided(t_statistic,t_critical)
comparePvalueAlpha(p_value,alpha)


#  --------- automatic test -------------

#t = 0.9636, df = 42.901, p-value = 0.8297
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#  -Inf 98.82894
# sample estimates:
# mean of x mean of y 
# 410.0796  374.0718 

# t = 0.9636 is the t_statistic, not the critical.


# two sided/tailed
t.test(
  x = participants,
  y = non_participants,
  alternative = "two.sided",
  conf.level = 1-alpha)

# right sided/tailed
t.test(
  x = participants,
  y = non_participants,
  alternative = "greater",
  conf.level = 1-alpha)

# left sided/tailed

t.test(
  x = participants,
  y = non_participants,
  alternative = "less",
  conf.level = 1-alpha)




