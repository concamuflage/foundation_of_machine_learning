# Load needed package
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

set.seed(123)

n <- 20               # sample size
n_sims <- 100000      # number of simulations
df <- n - 2           # degrees of freedom
t_values <- numeric(n_sims)

for (i in 1:n_sims) {
  # Generate two independent normal variables
  x <- rnorm(n)
  y <- rnorm(n)
  
  # Sample correlation
  r <- cor(x, y)
  
  # Test statistic
  t_values[i] <- r * sqrt((n - 2) / (1 - r^2))
}

# Put in a data frame for ggplot2
sim_df <- data.frame(t_values = t_values)

# Create density plot comparing simulated t-values to theoretical t-distribution
ggplot(sim_df, aes(x = t_values)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "white", fill = "skyblue", alpha = 0.6) +
  stat_function(fun = dt, args = list(df = df), color = "red", size = 1.2) +
  xlim(-6, 6) +
  labs(title = paste("Test statistic vs Student's t (df =", df, ")"),
       x = "t statistic", y = "Density") +
  theme_minimal()