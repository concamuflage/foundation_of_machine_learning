
tapply(X, INDEX, FUN)

tapply(ToothGrowth$len, ToothGrowth$supp, length) # group by one factor.
tapply(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose), length) # group by two factors

split_data = split(data,data$group) # split a dataframe by group
