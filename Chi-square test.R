# --- Chi-square Goodness-of-Fit ---

# Observed counts (use ONE categorical variable)
observed <- as.numeric(table(df_mf$sleep_score_cat))

# Total sample size
n_total <- sum(observed)

# Number of categories
k <- length(observed)

# Expected counts (equal distribution)
expected <- rep(n_total / k, k)

# Chi-square statistic
chi_sq_gof <- sum((observed - expected)^2 / expected)

# Degrees of freedom
df_gof <- k - 1

# p-value
p_value_gof <- 1 - pchisq(chi_sq_gof, df_gof)

# Output
cat("\nChi-square Goodness-of-Fit Test\n")
cat("Test Statistic:", chi_sq_gof, "\n")
cat("Degrees of Freedom:", df_gof, "\n")
cat("p-value:", p_value_gof, "\n")

# Decision
if (p_value_gof < 0.05) {
  cat("Reject H0: Sleep score distribution is not equal across categories\n")
} else {
  cat("Fail to reject H0: Sleep score distribution is equal across categories\n")
}