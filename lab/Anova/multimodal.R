
# Setup:
# this simple simulation demonstrates that
# when 5000 samples(each sample contains 20 sample elements) are drawn from 5000 populations,
# suppose the populations are from 5000 normal distributions of the same variance.

# each of the 5000 distributions has the same mu
# if the distribution of each population has the same mean as other distributions
# then the central limit theorem can apply and the collection of mean of each sample forms a normal
# distribution of (mu, variance/n). The yellow region

# each of the 5000 distributions has different mu
# else: the collection of sample means doesn't form a normal distribution at all- the blue region
# note: the sample means from one population is still a random normal variable. However, if we mix all the 
# samples means from all population, then, this random variable is no longer normal. 

set.seed(42)

# Parameters
sigma <- 10
n <- 20       # sample size per group
m <- 5000     # number of groups (sample means)
mu <- 50      # population mean under H0

# Case 1: H0 true (all populations have same mean)
means_H0 <- replicate(m, mean(rnorm(n, mean = mu, sd = sigma)))

# Case 2: H0 false (each group has different mean)
mus <- runif(m, 40, 60)  # true means vary between 40 and 60
means_H1 <- sapply(mus, function(mu_i) mean(rnorm(n, mean = mu_i, sd = sigma)))

# Range for plotting
x_range <- seq(min(means_H1), max(means_H1), length.out = 300)

# Theoretical normal density under H0
pdf_theoretical <- dnorm(x_range, mean = mu, sd = sigma / sqrt(n))

# Plot histograms
hist(means_H0, breaks = 40, freq = FALSE, col = rgb(1, 0.8, 0, 0.6),
     xlim = range(x_range), xlab = expression(bar(X)[i]),
     main = expression(paste("Distribution of Sample Means (", bar(X)[i], ") under H"[0], " vs H"[1])))

hist(means_H1, breaks = 40, freq = FALSE, col = rgb(0, 0.6, 1, 0.4), add = TRUE)

# Add theoretical density curve
lines(x_range, pdf_theoretical, col = "red", lwd = 2)

# Add reference lines
abline(v = mean(means_H0), col = "goldenrod", lty = 2, lwd = 1.5)
abline(v = mean(means_H1), col = "skyblue", lty = 2, lwd = 1.5)

# Add legend
legend("topright",
       legend = c(expression(H[0]~"true: same"~mu~"→ N(μ, σ²/n)"),
                  expression(H[1]~"true: μ varies across groups"),
                  expression("Theoretical N(μ, σ²/n) under H"[0])),
       fill = c(rgb(1, 0.8, 0, 0.6), rgb(0, 0.6, 1, 0.4), NA),
       border = NA,
       lty = c(NA, NA, 1),
       col = c(NA, NA, "red"),
       lwd = c(NA, NA, 2))