
# -----------------change section --------
data = read.csv("data/smoking_SBP.csv")
vector_value = data$SBP
vector_group = data$group
# -----------------change section --------


# --------------------Without Assuming Equal Variances---------------------------------

# using the built in method
pairwise.t.test(vector_value,vector_group,p.adjust = "none",pool.sd = FALSE) # pool.sd must be set to FALSE!

# --------------------Assuming Equal Variances---------------------------------

# Unjusted 
pairwise.t.test(vector_value,vector_group,p.adjust = "none",pool.sd = TRUE) 

# Bonferroni procedure
pairwise.t.test(vector_value,vector_group,p.adjust = "bonferroni",pool.sd = TRUE) 
# Tukey procedure
model = aov(vector_value ~ vector_group,data = data)
TukeyHSD(model)