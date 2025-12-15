
# -----------------change section --------
data = read.csv("data/smoking_SBP.csv")

vector_value = data$SBP
vector_group = data$grpnum
vector_group = factor(vector_group) # make it a factor, especially necessary if the groups are designated by numbers.

# -----------------change section -----------


# --------------------Without Assuming Equal Variances---------------------------------

# using the built in method
pairwise.t.test(vector_value,vector_group,p.adjust = "none",pool.sd = FALSE) # pool.sd must be set to FALSE!

# --------------------Assuming Equal Variances---------------------------------

# Unjusted 
pairwise.t.test(vector_value,vector_group,p.adjust = "none",pool.sd = TRUE) 

# Bonferroni procedure: the numbers in the result are the adjusted p values for our pairwise test.
pairwise.t.test(vector_value,vector_group,p.adjust = "bonferroni",pool.sd = TRUE) 
# Tukey procedure
model = aov(vector_value ~ vector_group,data = data)
TukeyHSD(model)

#         diff       lwr         upr     p adj
# 1-0  -8.25000 -20.96090   4.4608964 0.2811214

# -8.25000 difference in the mean 1-0
# -20.96090 lower bound of the CI for the difference


