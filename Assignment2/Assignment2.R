setwd("~/Desktop/HHN/Sem-6/courses/CPC351/CPC351-repo/Assignment2") # Set working directory!!

#
# Imports:
#
library(ggplot2)

#
# Question 1: Read the food nutrition data set and print structure/summary statistics.
#
food_nutrition_df <- read.csv("Data/Food_Nutrition_Dataset.csv")

str(food_nutrition_df)
head(food_nutrition_df, 10)
summary(food_nutrition_df)

#
# Question 2: Check for missing values:
#
food_nutrition_values <- food_nutrition_df[,3:8]

na_values <- colSums(is.na(food_nutrition_values))
barplot(na_values, las = 2, ylab = "Number of NA-values", main = "Q2: NA-values per column")

blank_values <- sapply(food_nutrition_values, function(x) {
  if (is.character(x)) sum(trimws(x) == "", na.rm = TRUE) else 0
})
blank_values
barplot(blank_values, las = 2, ylab = "Number of blank-values", main = "Q2: Blank-values per column")


zero_values <- sapply(food_nutrition_values, function(col) {
  sum(!is.na(col) & col == 0)
})
barplot(zero_values, las = 2, ylab = "Number of zero-values", main = "Q2: Zero-values per column")

# Total missing values including NA and white spaces (excluding 0.0-values)
missing_values <- na_values + blank_values
barplot(missing_values, las = 2, ylab = "Number of missing values", main = "Q2: Missing values per column")

# Print a table of all 0-value entries:
entry_name <- food_nutrition_df[[1]]
to_num <- function(v) suppressWarnings(as.numeric(as.character(v)))
zero_pos <- which(sapply(food_nutrition_values, function(col) {
  v <- to_num(col)
  !is.na(v) & v == 0
}), arr.ind = TRUE)

zero_table <- data.frame(
  Name   = entry_name[zero_pos[, "row"]],
  Column = colnames(food_nutrition_values)[zero_pos[, "col"]],
  Id     = zero_pos[, "row"],
  Value  = food_nutrition_values[zero_pos]
)

# View(zero_table)
zero_table
# The table containing entries with 0.0 values does not allow for any conclusions 
# regarding incorrect data, therefore the results are not included in the overall statistics!

#
# Question 3: Display the number of unique food categories.
#
ggplot(food_nutrition_df, aes(x = food_nutrition_df[[2]])) +
  geom_bar() +
  coord_flip() +
  labs(x = "Category", y = "Frequency", title = "Q3: Number of unique food categories") +
  theme_minimal()

#
# Question 4: Find the top 10 foods with the highest calories.
#
top10_calories <- food_nutrition_df[order(food_nutrition_df[[3]], decreasing = TRUE), c(1, 3)]
top10_calories <- top10_calories[!is.na(top10_calories[[2]]), ]

top10_calories <- head(top10_calories, 10)
names(top10_calories) <- c("Name", "Calories")

# View(top10_calories)
top10_calories

#
# Question 5: Calculate the average calories, protein, carbs, and fat per category.
#
avg_by_category <- aggregate(
  food_nutrition_df[, c("calories", "protein", "carbs", "fat")],
  by = list(category = food_nutrition_df$category),
  FUN = function(x) mean(x, na.rm = TRUE)
)

avg_by_category <- avg_by_category[order(avg_by_category$calories, decreasing = TRUE), ]


# View(avg_by_category)
avg_by_category

#
# Question 6: Create a histogram of calories for all foods.
#
ggplot(food_nutrition_df, aes(x = calories)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  labs(
    title = "Q6: Histogram of Calories for all foods",
    x = "Calories",
    y = "Number of foods"
  ) +
  theme_minimal()

#
# Question 7: Plot a boxplot of calories grouped by category.
#
ggplot(food_nutrition_df, aes(x = category, y = calories)) +
  geom_boxplot(na.rm = TRUE) +
  coord_flip() +
  labs(
    title = "Q7: Calories by category",
    x = "Category",
    y = "Calories"
  ) +
  theme_minimal()

#
# Question 8: Create a scatter plot of calories vs protein, color-coded by category.
#
ggplot(food_nutrition_df, aes(x = protein, y = calories, color = category)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  labs(
    title = "Q8: Calories vs Protein",
    x = "Protein",
    y = "Calories",
    color = "Categories"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

library(ggplot2)

library(dplyr)

# Q11:  Identify foods with extremely high fat content (above 95th percentile) and visualize them. 
# Step 1: Compute the 95th percentile of fat content.
fat_p95 <- quantile(food_nutrition_df$fat, probs = 0.95, na.rm = TRUE)

# Step 2: Filter foods whose fat content exceeds the 95th percentile.

high_fat <- food_nutrition_df %>%
  filter(!is.na(fat), fat > fat_p95)

# Step 3: Visualize the high-fat foods using a horizontal bar chart.
ggplot(high_fat, aes(x = reorder(food_name, fat), y = fat)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Foods with Extremely High Fat Content (>95th Percentile)",
    subtitle = paste("95th percentile fat threshold =", round(fat_p95, 2)),
    x = "Food",
    y = "Fat Content (g)"
  ) +
  theme_minimal()

# Q12: Compare the distribution of carbs across three selected categories using a violin plot. 
# Step 1: Select three representative food categories.
top3_categories <- food_nutrition_df %>%
  count(category, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(category)

# Step 2: Subset the data for the selected categories and remove missing values.
df_3cats <- food_nutrition_df %>%
  filter(category %in% top3_categories, !is.na(carbs))

# Step 3: Use a violin plot to visualize the distribution of carbohydrates.
ggplot(df_3cats, aes(x = category, y = carbs, fill = category)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white") +
  labs(
    title = "Distribution of Carbohydrates Across Selected Categories",
    x = "Category",
    y = "Carbohydrates (g)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Q13: Create a stacked bar chart showing total calories contributed by each category. 
# Step 1: Aggregate total calories for each food category.
calories_by_category <- food_nutrition_df %>%
  group_by(category) %>%
  summarise(total_calories = sum(calories, na.rm = TRUE), .groups = "drop")

# Step 2: Visualize the contribution of each category using a stacked bar chart.
ggplot(calories_by_category, aes(x = "All Foods", y = total_calories, fill = category)) +
  geom_col() +
  labs(
    title = "Total Calories Contributed by Each Category",
    x = NULL,
    y = "Total Calories"
  ) +
  theme_minimal()

# Q14: Plot a density curve for protein content across all foods. 
# Step 1: Remove missing values in protein to ensure a valid density estimate.
df_protein <- food_nutrition_df %>% filter(!is.na(protein))

# Step 2: Plot a density curve to visualize the overall distribution of protein.
ggplot(df_protein, aes(x = protein)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    title = "Density Curve of Protein Content Across All Foods",
    x = "Protein Content (g)",
    y = "Density"
  ) +
  theme_minimal()

# Q15: Create a bubble chart where bubble size represents iron content, and axes are calories vs carbs.
# Step 1: Prepare a clean dataset 
df_bubble <- food_nutrition_df %>%
  filter(!is.na(calories), !is.na(carbs), !is.na(iron))

# Step 2: Create a bubble chart using a scatter plot.
ggplot(df_bubble, aes(x = carbs, y = calories, size = iron)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  labs(
    title = "Bubble Chart of Calories vs Carbohydrates",
    subtitle = "Bubble size represents iron content",
    x = "Carbohydrates (g)",
    y = "Calories",
    size = "Iron Content"
  ) +
  theme_minimal()

# Q16
library(tidyverse)
# 1) Read data
df <- read.csv("/Users/sherlock/Desktop/Y3S1/351/A2/Food_Nutrition_Dataset.csv")

# 2) Calculate the calories (kcal) of the three macronutrients.
df2 <- df %>%
  mutate(
    protein_kcal = protein * 4,
    carbs_kcal   = carbs * 4,
    fat_kcal     = fat * 9,
    total_macro_kcal = protein_kcal + carbs_kcal + fat_kcal
  )
# =========================
# Q16 Nutrient Ratio Analysis
# a) protein-to-calorie ratio
# b) top 10 foods
# c) horizontal bar chart
# =========================

q16_top10 <- df2 %>%
  mutate(protein_to_cal_ratio = protein / calories) %>%  # g per kcal
  arrange(desc(protein_to_cal_ratio)) %>%
  slice_head(n = 10) %>%
  select(food_name, category, calories, protein, protein_to_cal_ratio)

print(q16_top10)

# Horizontal bar chart (Top 10)
p16 <- q16_top10 %>%
  # Before plotting, reorder the food_name factors by ratio.
  mutate(food_name = reorder(food_name, protein_to_cal_ratio)) %>% 
  
  ggplot(aes(x = food_name, y = protein_to_cal_ratio)) +
  
  # 1. Use geom_col and fill the color according to the ratio, without displaying the legend.
  geom_col(aes(fill = protein_to_cal_ratio), show.legend = FALSE, width = 0.7) +
  
  # 2. Add numerical labels
  # label = round(...) This will round the value to 3 decimal places.
  # hjust = -0.2 Position the label slightly to the right of the top of the pillar.
  geom_text(aes(label = round(protein_to_cal_ratio, 3)), 
            hjust = -0.2, 
            size = 3.5, 
            color = "#333333") + 
  
  # 3. Flip the coordinate axes
  coord_flip() +
  
  # 4. Set a color gradient 
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  
  # 5. Expand the Y-axis range to make room for the numerical labels on the right and prevent them from being cut off.
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  
  # 6. Set title and axis labels
  labs(
    title = "Top 10 Foods by Protein-to-Calorie Ratio",
    subtitle = "Highest protein efficiency (g/kcal)",
    x = NULL, 
    y = "Protein-to-Calorie Ratio (g per kcal)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  )
print(p16)

# Q17
# 1. Calculate and organize the data
q17_cat <- df2 %>%
  group_by(category) %>%
  summarise(
    protein_kcal = sum(protein_kcal, na.rm = TRUE),
    carbs_kcal   = sum(carbs_kcal,   na.rm = TRUE),
    fat_kcal     = sum(fat_kcal,     na.rm = TRUE),
    total_kcal_from_macros = protein_kcal + carbs_kcal + fat_kcal,
    .groups = "drop"
  ) %>%
  mutate(
    protein_pct = protein_kcal / total_kcal_from_macros * 100,
    carbs_pct   = carbs_kcal   / total_kcal_from_macros * 100,
    fat_pct     = fat_kcal     / total_kcal_from_macros * 100
  )

# 2. Sort by category based on protein content
q17_cat <- q17_cat %>%
  mutate(category = reorder(category, protein_pct)) 

# 3. Convert to long table
q17_long <- q17_cat %>%
  select(category, protein_pct, carbs_pct, fat_pct) %>%
  pivot_longer(
    cols = c(protein_pct, carbs_pct, fat_pct),
    names_to = "macronutrient",
    values_to = "percent"
  ) %>%
  mutate(
    macronutrient = recode(macronutrient,
                           protein_pct = "Protein",
                           carbs_pct   = "Carbs",
                           fat_pct     = "Fat"),
    macronutrient = factor(macronutrient, levels = c("Fat", "Carbs", "Protein"))
  )

# 4. Stacked bar chart
p17 <- q17_long %>%
  ggplot(aes(x = category, y = percent, fill = macronutrient)) +
  
  # Bar chart
  geom_col(width = 0.8, alpha = 0.9) + 
  
  # Add percentage value labels
  geom_text(aes(label = ifelse(percent > 3, paste0(round(percent, 0), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 3,
            fontface = "bold") +
  
  # Flip the axes
  coord_flip() +
  
  scale_fill_manual(values = c(
    "Protein" = "#FF6B6B",  
    "Carbs"   = "#FFD93D",  
    "Fat"     = "#4D96FF"   
  )) +
  
  # Axis and title settings
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0,0)) + 
  labs(
    title = "Macronutrient Composition by Category",
    subtitle = "Categories ordered by Protein contribution",
    x = NULL, 
    y = "Percentage of Total Calories",
    fill = "Macro"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "top",          
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),   
    axis.text.y = element_text(size = 10, color = "black"), 
    plot.title = element_text(face = "bold", size = 14)
  )

print(p17)
