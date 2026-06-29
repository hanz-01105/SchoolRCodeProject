library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# -----------------------------
# LOAD DATA (ONLY ONCE)
# -----------------------------
df <- read.csv("sleep_mobile_stress_dataset_15000.csv", stringsAsFactors = FALSE)

# -----------------------------
# NUMERICAL DATA ANALYSIS
# -----------------------------

summary(df)

# DO NOT overwrite functions like mean(), sd()
mean_val <- mean(df$sleep_quality_score)
median_val <- median(df$sleep_quality_score)
sd_val <- sd(df$sleep_quality_score)
IQR_val <- IQR(df$sleep_quality_score)
min_val <- min(df$sleep_quality_score)
max_val <- max(df$sleep_quality_score)

# Histogram
hist <- df %>%
  ggplot(aes(x = sleep_quality_score)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = median_val, color = "red", linetype = "dashed") +
  ggtitle("Sleep Quality Score Histogram")

hist

# Assumptions
independence <- TRUE
normality <- nrow(df) > 30

# Outliers
outlier_bounds <- mean_val + c(-1,1) * 1.5 * IQR_val
lower_outlier <- df$sleep_quality_score[df$sleep_quality_score < outlier_bounds[1]]
upper_outlier <- df$sleep_quality_score[df$sleep_quality_score > outlier_bounds[2]]

# One-sample t-test (MANUAL)
n <- nrow(df)
dof <- n - 1
SE <- sd_val / sqrt(n)

# USE MEAN (not median)
test_stat <- (mean_val - 5) / SE
pval <- pt(test_stat, dof, lower.tail = FALSE)
reject_null <- pval < 0.05

cat("\nOne-sample t-test\n")
cat("t:", test_stat, "\n")
cat("df:", dof, "\n")
cat("p-value:", pval, "\n")

# -----------------------------
# CATEGORICAL ANALYSIS
# -----------------------------

# Keep only Male and Female
df_mf <- df %>% filter(gender %in% c("Male", "Female"))

# Ensure numeric
df_mf$sleep_quality_score <- as.numeric(df_mf$sleep_quality_score)

# Categorize
df_mf <- df_mf %>%
  mutate(
    sleep_score_cat = case_when(
      sleep_quality_score < 4 ~ "Low (< 4)",
      sleep_quality_score >= 4 & sleep_quality_score < 7 ~ "Moderate (4-7)",
      sleep_quality_score >= 7 ~ "High (>= 7)"
    ),
    sleep_score_cat = factor(sleep_score_cat,
                             levels = c("Low (< 4)", "Moderate (4-7)", "High (>= 7)")
    )
  )

# Contingency table
ct <- table(df_mf$gender, df_mf$sleep_score_cat)
print(addmargins(ct))

# Summary stats by gender
summary_stats <- df_mf %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    Mean = round(mean(sleep_quality_score), 3),
    Median = round(median(sleep_quality_score), 3),
    SD = round(sd(sleep_quality_score), 3),
    Min = min(sleep_quality_score),
    Max = max(sleep_quality_score),
    .groups = "drop"
  )

print(summary_stats)

# Bar chart
df_ct <- as.data.frame(ct)
colnames(df_ct) <- c("Gender", "Sleep_Score_Category", "Count")

p1 <- ggplot(df_ct, aes(x = Sleep_Score_Category, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.65) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.65),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Female" = "#E07B8A", "Male" = "#5B8DB8")) +
  labs(
    title = "Sleep Quality Score Category by Gender",
    x = "Sleep Quality Score Category",
    y = "Count",
    fill = "Gender"
  ) +
  theme_minimal()

print(p1)

# -----------------------------
# CHI-SQUARE GOODNESS-OF-FIT
# -----------------------------

observed <- as.numeric(table(df_mf$sleep_score_cat))
n_total <- sum(observed)
k <- length(observed)
expected <- rep(n_total / k, k)

chi_sq_gof <- sum((observed - expected)^2 / expected)
df_gof <- k - 1
p_value_gof <- 1 - pchisq(chi_sq_gof, df_gof)

cat("\nChi-square Goodness-of-Fit Test\n")
cat("Test Statistic:", chi_sq_gof, "\n")
cat("Degrees of Freedom:", df_gof, "\n")
cat("p-value:", p_value_gof, "\n")

if (p_value_gof < 0.05) {
  cat("Reject H0: Sleep score distribution is not equal across categories\n")
} else {
  cat("Fail to reject H0: Sleep score distribution is equal across categories\n")
}