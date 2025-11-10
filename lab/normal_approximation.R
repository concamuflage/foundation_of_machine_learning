# Clear environment and graphics
rm(list = ls())
graphics.off()

plot_binomial_vs_normal <- function(n, p) {
  x <- 0:n
  binom_probs <- dbinom(x, n, p)
  mean <- n * p
  sd <- sqrt(n * p * (1 - p))
  
  bar_centers <- barplot(binom_probs, names.arg = x, space = 0,
                         main = sprintf("n=%d, p=%.2f (np=%.1f, n(1-p)=%.1f)", 
                                        n, p, n*p, n*(1-p)),
                         xlab = "Number of successes", ylab = "Probability",
                         col = "skyblue", border = "white")
  
  x_cont <- seq(0, n, 0.1)
  lines(x_cont, dnorm(x_cont, mean, sd), col = "red", lwd = 2)
  
  legend("topright", legend = c("Binomial", "Normal approximation"), 
         fill = c("skyblue", NA), border = NA, lty = c(NA, 1), col = c("black", "red"))
}

# Run each manually to zoom and inspect clearly
plot_binomial_vs_normal(100, 0.03)
plot_binomial_vs_normal(100, 0.9)
plot_binomial_vs_normal(50, 0.5)
plot_binomial_vs_normal(200, 0.2)
