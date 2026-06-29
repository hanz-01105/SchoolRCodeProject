library(ggplot2)
library(tidyverse)
library(skimr)
library(gtsummary)
custom_colors <- c("#6E32FE", "#FFDE5E", "#036C6B", "#FE8027")
df <- read.csv("customer_segmentation_mixed.csv")


# Contingency Table
ct <- table(df$Gender, df$Preferred_Product)
ct
addmargins(ct) # add sum columns

# Pie Chart
table_data <- df %>% 
  count(Preferred_Product) %>%
  mutate(Preferred_Product = factor(Preferred_Product))

table_data <- table_data %>%
  mutate(prop = n/sum(n),
         label = paste0(round(prop * 100, 1), "%"))

ggplot(data = table_data, aes(x="", y=n, fill=Preferred_Product)) +
  geom_col() +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5),
            inherit.aes = TRUE) +  # Explicitly inherit aesthetics
  coord_polar(theta="y") +
  scale_fill_manual(values = c("Clothing" = "#6E32FE", 
                               "Electronics" = "#FFDE5E", 
                               "Groceries" = "#036C6B", 
                               "Home"  = "#FE8027", 
                               "Sports"  = "#8389FF")) +
  theme_void()+
  labs(title = "Customer's Preferred Product", 
       fill = "Preferred Product")

# Gender Bar Plot
ggplot(df, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  geom_text(stat = "count", 
            aes(label = ..count..), 
            vjust = -0.2, 
            size = 4) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Customer Distribution by Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Scatter Plot Age vs Spending Score
ggplot(df, aes(x = Age, y = Spending_Score, color = Gender)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Female" = "purple", "Male" = "blue")) +
  labs(title = "Age vs Spending Score (By Gender)",
       x = "Age (years old)", y = "Spending Score") +
  theme_minimal()

# Scatter Plot Annual Income vs Spending Score
ggplot(df, aes(x = Annual_Income, y = Spending_Score, color = Preferred_Product)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Annual Income vs Spending Score (By Preferred Product)",
       x = "Annual Income (Dollar)", y = "Spending Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Summary Statistic Table
skim(df)
df %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",   
      all_categorical() ~ "{n} ({p}%)"      
    ),
    digits = all_continuous() ~ 1            
  ) %>%
  add_n() %>%                                 
  modify_header(label = "**Variable**") %>%   
  bold_labels()   


df_dropped <- df %>%
  drop_na()

glimpse(df_dropped)
names(df_dropped)
head(df_dropped)

# Distribution Histogram
plot_age <- ggplot(data=df_dropped, aes(Age)) + 
  geom_histogram(binwidth = 5, color='black', fill = 'orange') +
  labs(x='Age', y='Count', title = 'Distribution of Age')

plot_income <- ggplot(data=df_dropped, aes(Annual_Income)) + 
  geom_histogram(binwidth = 10000, color='black', fill = 'skyblue') +
  labs(x='Annual Income', y='Count', title = 'Distribution of Annual Income')

plot_score <- ggplot(data=df_dropped, aes(Spending_Score)) + 
  geom_histogram(binwidth = 5, color='black', fill = 'purple') +
  labs(x='Spending Score', y='Count', title = 'Distribution of Spending Score')

plot_visits <- ggplot(data=df_dropped, aes(Monthly_Visits)) + 
  geom_histogram(binwidth = 1,boundary = 0.5, color='black', fill = 'yellow') +
  labs(x='Monthly Visits', y='Count', title = 'Distribution of Monthly Visits')

print(plot_age)
print(plot_income)
print(plot_score)
print(plot_visits)

# Box Plot
plot_box <- ggplot(data=df_dropped, aes(x = Gender, y = Monthly_Visits, fill = Gender)) +
  geom_boxplot() + 
  labs(
    title = "Monthly visits by Gender",
    x = "Gender",
    y = "Monthly visits"
  )

print(plot_box)







