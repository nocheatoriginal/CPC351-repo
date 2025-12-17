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



zero_values <- sapply(food_nutrition_values, function(col) {
  sum(!is.na(col) & col == 0)
})

barplot(zero_values, las = 2, ylab = "Number of zero-values", main = "Q2: Zero-values per column")

# Total missing values including NA and 0.0
missing_values <- na_values + zero_values
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
