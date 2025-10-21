# ============================================================
# Econ115a Midterm Project Answer Key
# Shopping Behavior Dataset
# ============================================================

# 📦 Load Required Libraries
library(tidyverse)

# ============================================================
# 📁 Task 1: Load and Inspect the Dataset
shopping_data <- read_csv("shopping_behavior.csv")

str(shopping_data)
summary(shopping_data)
head(shopping_data)

# Observation count
nrow(shopping_data)

# Key variables: Age, Gender, Category, Purchase Amount (USD), Season

# ============================================================
# 🧹 Task 2: Data Cleaning
# Check for missing values
colSums(is.na(shopping_data))

# Recode categorical variables
shopping_data <- shopping_data %>%
  mutate(
    Gender = factor(Gender),
    Category = factor(Category),
    Season = factor(Season)
  )

# ============================================================
# 📊 Task 3: Purchase Amount Distribution
ggplot(shopping_data, aes(x = `Purchase Amount (USD)`)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Purchase Amounts",
       x = "Purchase Amount (USD)",
       y = "Frequency")

# Interpretation:
# Most purchases fall between $40 and $80, with a peak around $60.

# ============================================================
# 👥 Task 4: Spending by Gender
ggplot(shopping_data, aes(x = Gender, y = `Purchase Amount (USD)`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Spending by Gender",
       x = "Gender",
       y = "Purchase Amount (USD)")

# Interpretation:
# Female shoppers show slightly higher median spending, with more variability.

# ============================================================
# 📦 Task 5: Spending by Product Category
shopping_data %>%
  group_by(Category) %>%
  summarise(avg_spend = mean(`Purchase Amount (USD)`)) %>%
  ggplot(aes(x = reorder(Category, avg_spend), y = avg_spend)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(title = "Average Spending by Product Category",
       x = "Category",
       y = "Average Purchase Amount (USD)")

# Interpretation:
# Clothing and Accessories are top spend categories.

# ============================================================
# 📅 Task 6: Seasonal Spending Patterns
shopping_data %>%
  group_by(Season) %>%
  summarise(total_spend = sum(`Purchase Amount (USD)`)) %>%
  ggplot(aes(x = Season, y = total_spend)) +
  geom_col(fill = "orchid") +
  labs(title = "Total Spending by Season",
       x = "Season",
       y = "Total Purchase Amount (USD)")

# Interpretation:
# Spring and Fall show higher total spending.

# ============================================================
# 🎂 Task 7: Age Group Analysis
shopping_data <- shopping_data %>%
  mutate(AgeGroup = cut(Age, breaks = c(18, 25, 35, 45, 55, 70),
                        labels = c("18–25", "26–35", "36–45", "46–55", "56–70")))

shopping_data %>%
  group_by(AgeGroup) %>%
  summarise(avg_spend = mean(`Purchase Amount (USD)`)) %>%
  ggplot(aes(x = AgeGroup, y = avg_spend)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average Spending by Age Group",
       x = "Age Group",
       y = "Average Purchase Amount (USD)")

# Interpretation:
# Shoppers aged 26–35 and 36–45 tend to spend more.

# ============================================================
# 🧾 Task 8: Annotated Insight
ggplot(shopping_data, aes(x = `Purchase Amount (USD)`)) +
  geom_histogram(binwidth = 5, fill = "steelblue") +
  geom_vline(xintercept = 60, color = "red", linetype = "dashed") +
  annotate("text", x = 65, y = 100, label = "Peak spending zone", color = "red") +
  labs(title = "Annotated Purchase Distribution",
       x = "Purchase Amount (USD)",
       y = "Frequency")

# Insight explanation:
# Most purchases cluster around $60, suggesting a pricing sweet spot for promotions.

# ============================================================
# 💾 Task 9: Save Your Visuals
ggsave("purchase_distribution.png", width = 7, height = 5)
ggsave("gender_boxplot.png", width = 7, height = 5)
ggsave("category_spending.png", width = 7, height = 5)

# ============================================================
# 📝 Task 10: Reflection
# """
# Data visualization helps retailers understand customer behavior at a glance.
# By comparing spending across gender, age groups, and seasons, businesses can tailor promotions and inventory to match demand.
# For example, identifying peak spending in spring or among 26–35 year-olds allows for targeted campaigns.
# Visual tools also make it easier to communicate insights across teams and support data-driven decisions.
# """

# ✅ End of Answer Key
