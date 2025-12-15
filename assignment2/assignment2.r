

qt(0.95,21)

cat(qt(0.975,24))

# (1) Summarize the data by whether children participated in the meal preparation or not. Use an
#appropriately labelled table to show the results. Also include a graphical presentation that shows the
#distribution of calories for participants vs. non-participants. Describe the shape of each distribution and
#comment on the similarity (or lack thereof) between the distributions in each group.

df <- read.csv("calories.csv")

participants <- df$participants
non_participants <- df$non_participants
non_participants <- na.omit(non_participants)



hist(
  participants,
  breaks = 50,
  main = "Distribution of calories for participants",
  xlab = "values",
  ylab = "frequencies",
  col = "gray",
  border = "white"
)

hist(
  non_participants,
  breaks = 50,
  main = "Distribution of calories for non_participants",
  xlab = "values",
  ylab = "frequencies",
  col = "gray",
  border = "white"
)

t.test(
    x = participants,
    y = non_participants,
    alternative = "greater",
    conf.level = 0.95)

cat(sd(participants),"\n")
cat(sd(non_participants))

# to test the null hypothesis of mean = 425
t.test(participants,mu = 425)
# to test the sample mean with a confidence level of 90%.
t.test(participants,conf.level = 0.9)

qt(0.95,4)

qt(0.95,42.901)

pnorm(0.95)
qt(0.95,24) 

cat(sd(participants))
cat(sd(non_participants))

summary(participants)
summary(non_participants)





