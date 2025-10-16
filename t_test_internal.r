



t_statistic = ( sample_mean - null_mean ) / (sample_sd/ sqrt(sample_size))

p_value = 1 - pt(t_statistic,degree_of_freedom)

res <- t.test(vector, mu = 20, alternative = "greater", conf.level = 0.90)
cat("\n--- t.test summary ---\n")
cat("t =", as.numeric(res$statistic),
    "df =", as.numeric(res$parameter),
    "p-value =", res$p.value, "\n")
cat("One-sided 90% CI (lower bound):", res$conf.int[1], "\n")