df <- read.csv("states.csv")
ml(state ~ metro_res, white,hs_grad,poverty,female_house)


pbinom(12,20,0.4)