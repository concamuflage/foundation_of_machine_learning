
#standard normal

# Z < a 
a = 1.5 # change
pnorm(a)

# Z > a # change
a = 0.84
1-pnorm(a)

# b  < Z < a 
b = -1.5 # change
a = 2.5 # change
pnorm(a) - pnorm(b)

# |Z| > a
a = 1.8
pnorm(-a) + (1 - pnorm(a))

# |Z| < a
a = 0.5270463
pnorm(a)-pnorm(-a)

# normal
mean_population = 100   # change
sd_population = 14.2    # change
number = 90

# < number
less_than_number = pnorm(number,mean =mean_population,sd = sd_population)
less_than_number
# > number
greater_than_number = 1-less_than_number
greater_than_number


#poisson

#binomial

#uniform
