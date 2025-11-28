#	Quantitative interaction = same direction, different magnitude
# Qualitative interaction = opposite direction (cross-over interaction, effect reverses)

5 steps:
  
1 Write the hypothesis and alpha level 
2 Choose statistics
3 Rejection rule
4 Calculation
5 Conclusion


data$temp_level = ifelse(data$temp>=98.6,1,0)

male_data = filter(data,sex ==1)

nrow(subset(data, sex == 1 & temp_level == 1))

