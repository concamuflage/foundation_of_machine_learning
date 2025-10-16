vector <- c(18.1, 23.4, 23.8, 24.1, 22.5, 19, 25.4, 23.1, 16.5, 26.7)
sample_mean <- mean(vector)
sample_sd <- sd(vector)

sample_mean <- 3.2
sample_sd <- 4.5

sample_size <- 18
degree_of_freedom <- sample_size - 1
significance_level <- 0.1
null_mean <- 0
one_sided <- FALSE

standard_error <- sample_sd / sqrt(sample_size)

if (one_sided) {
    critical_value <- qt(1 - significance_level, degree_of_freedom)
} else {
    critical_value <- qt(1 - significance_level / 2, degree_of_freedom)
}

# lower = sample_mean - critical_value * standard_error
# higher = sample_mean + critical_value * standard_error

t_statistic <- (sample_mean - null_mean) / (sample_sd / sqrt(sample_size))


# res <- t.test(vector, mu = 20, alternative = "greater", conf.level = 0.90)
# cat("\n--- t.test summary ---\n")
# cat("t =", as.numeric(res$statistic),
#     "df =", as.numeric(res$parameter),
#     "p-value =", res$p.value, "\n")
# cat("One-sided 90% CI (lower bound):", res$conf.int[1], "\n")



if (abs(t_statistic) >= critical_value) {
    cat("critical value", critical_value, "\n")
    cat("t_statistic", t_statistic, "\n")
    cat("reject the null hypothesis", "\n")
} else {
    cat("fail to reject the null hypothesis")
}

# cat("confidence interval is", lower, higher)


# cat("p_value associated with the t_statistic is:", p_value)
