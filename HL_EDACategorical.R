library(ggplot2)
library(dplyr)
library(scales)

df <- read.csv("sleep_mobile_stress_dataset_15000.csv", stringsAsFactors = FALSE)

# Keep only Male and Female
df_mf <- df %>% filter(gender %in% c("Male", "Female"))

# Ensure sleep_quality_score is numeric
df_mf$sleep_quality_score <- as.numeric(df_mf$sleep_quality_score)

# Categorise sleep_quality_score into Low / Moderate / High
df_mf <- df_mf %>%
  mutate(
    sleep_score_cat = case_when(
      sleep_quality_score < 4                             ~ "Low (< 4)",
      sleep_quality_score >= 4 & sleep_quality_score < 7  ~ "Moderate (4-7)",
      sleep_quality_score >= 7                            ~ "High (>= 7)"
    ),
    sleep_score_cat = factor(sleep_score_cat,
                             levels = c("Low (< 4)", "Moderate (4-7)", "High (>= 7)"))
  )


# Contingency table with margins
ct <- table(Gender = df_mf$gender,
            Sleep_Score_Category = df_mf$sleep_score_cat)
print(addmargins(ct))

# Summary statistics by gender
summary_stats <- df_mf %>%
  group_by(gender) %>%
  summarise(
    n      = n(),
    Mean   = round(mean(sleep_quality_score), 3),
    Median = round(median(sleep_quality_score), 3),
    SD     = round(sd(sleep_quality_score), 3),
    Min    = min(sleep_quality_score),
    Max    = max(sleep_quality_score),
    .groups = "drop"
  )
print(as.data.frame(summary_stats))


# Grouped Bar Chart
df_ct <- as.data.frame(ct)
colnames(df_ct) <- c("Gender", "Sleep_Score_Category", "Count")

p1 <- ggplot(df_ct, aes(x = Sleep_Score_Category, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.65) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.65),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Female" = "#E07B8A", "Male" = "#5B8DB8")) +
  labs(
    title   = "Sleep Quality Score Category by Gender",
    x       = "Sleep Quality Score Category",
    y       = "Count",
    fill    = "Gender",
    caption = "Data: sleep_mobile_stress_dataset_15000.csv"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

print(p1)

