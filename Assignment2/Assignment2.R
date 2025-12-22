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

