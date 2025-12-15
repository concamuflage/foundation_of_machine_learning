
population_mean =  # change
population_sd = 3 # change
sample_size = 10 # change

x_bar_mean = population_mean
x_bar_sd = population_sd /sqrt(sample_size)


pnorm(206, mean = x_bar_mean, sd = x_bar_sd) - pnorm(198, mean = x_bar_mean, sd = x_bar_sd) # change