data = read.csv("data/golf_distance.csv")

# --------------------Without Assuming Equal Variances---------------------------------
# using the built in method
pairwise.t.test(data$distance,data$brand,p.adjust = "none",pool.sd = FALSE) # pool.sd must be set to FALSE!

# --------------------Assuming Equal Variances---------------------------------

# Unjusted 
pairwise.t.test(data$distance,data$brand,p.adjust = "none",pool.sd = TRUE) 

# Bonferroni procedure
pairwise.t.test(data$distance,data$brand,p.adjust = "bonferroni",pool.sd = TRUE) 
# Tukey procedure
model = aov(data$distance ~ data$brand,data = data)
TukeyHSD(model)