################################
### lecture One-way ANOVA code - CS-555
### Data Analysis of Variance
### By: Farshid Alizadeh-Shabdiz
#### 


# Smoking data and its impact on blood pressure
data <- read.csv("https://raw.githubusercontent.com/kiat/R-Examples/master/Datasets/smoking_SBP.csv")


# Check if the variable group is factor variable in R.
# If not make it a factor variable, which means change catogorical to enumerated type
is.factor(data$group)
data$group = factor(data$group)
is.factor(data$group)

# aov(data$response~data$group)
m <- aov(data$SBP~data$group, data=data)

# pass the anova model object to the summary function.
summary(m)


##############################
### Pairwise t-test 
###


### EXAMPLE 1 ###
# Smoking Group
# Numerical and graphical summaries (Module 1 and 2)
# Calculate mean, SD of SBP by groups
# Box plots and histograms
aggregate(data$SBP, by=list(data$group), summary) # calculate summary stats of data subsets

aggregate(data$SBP, by=list(data$group), sd)

# Create a Boxplot 
boxplot(data$SBP~data$group, data=data, main="SBP by smoking status", xlab="group", ylab="SBP", ylim=c(100, 160))

# Perform one-way ANOVA and if necessary, calculate the associated pairwise comparisons
m<- aov(data$SBP~data$group, data=data)
summary(m)

# pairwise t-test 
pairwise.t.test(data$SBP, data$group, p.adj="none")

# pairwise t-test with bonferroni adjustment 
# bonferroni adjustment
pairwise.t.test(data$SBP, data$group, p.adj="bonferroni")

# Tukeys Test of Honest Significance Test
TukeyHSD(m)

### EXAMPLE 2 ###
#Golf Ball Groups
golf <- read.csv("https://raw.githubusercontent.com/kiat/R-Examples/master/Datasets/GolfBals.csv")
golf
attach(golf)

aggregate(distance, by=list(brand), summary)

pairwise.t.test(distance, brand, p.adj='none')

# Pairwise t test with bonferroni adjustment. 

pairwise.t.test(distance, brand, p.adj="bonferroni")

m<- aov(distance~brand, data=golf)
TukeyHSD(m)
