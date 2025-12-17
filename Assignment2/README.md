# CPC351 - Assignment 2

This repository contains our solutions for Assignment 2 of the CPC351 course. The `Assignment2.R` script performs a series of data exploration and visualization tasks based on the "Food_Nutrition_Dataset.csv" dataset.

## Problem Description

The assignment consists of 17 main tasks that involve data exploration and visualization using R:

1.  Load the dataset and display its structure and summary statistics.
2.  Check for missing values in each column and visualize them using a bar chart.
3.  Count the number of unique food categories and display them in a bar chart.
4.  Find the top 10 foods with the highest calories.
5.  Calculate the average calories, protein, carbs, and fat per category.
6.  Create a histogram of calories for all foods.
7.  Plot a boxplot of calories grouped by category.
8.  Create a scatter plot of calories vs protein, color-coded by category.
9.  Generate a correlation heatmap for numeric columns (calories, protein, carbs, fat, iron, vitamin_c).
10. Create a bar chart showing the top 10 categories by average vitamin C content.
11. Identify foods with extremely high fat content (above 95th percentile) and visualize them.
12. Compare the distribution of carbs across three selected categories using a violin plot.
13. Create a stacked bar chart showing total calories contributed by each category.
14. Plot a density curve for protein content across all foods.
15. Create a bubble chart where bubble size represents iron content, and axes are calories vs carbs.
16. **Nutrient Ratio Analysis**
    a. Calculate the protein-to-calorie ratio for each food item.
    b. Identify the top 10 foods with the highest ratio.
    c. Visualize these top foods using a horizontal bar chart.
17. **Macronutrient Contribution**
    a. For each category, compute the percentage contribution of protein, carbs, and fat to total calories.
    b. Create a stacked bar chart showing macronutrient composition by category.

## How to Run the Script

### 1. Data Setup

The `Data` directory, which contains the datasets for this assignment, is not included in this repository and is listed in the `.gitignore` file to avoid publishing sensitive or proprietary information.

To run the script, you must **manually add the `Food_Nutrition_Dataset.csv` file** to the `Data` folder. The expected structure is as follows:

```
CPC351_Assignment2/
├── Assignment2.R
├── README.md
└── Data/
    └── Food_Nutrition_Dataset.csv
```

### 2. Set Working Directory

The R script assumes that the working directory is set to the root of this project folder. If you run the script from a different context, you may need to set the working directory manually at the beginning of the `Assignment2.R` script.

You can do this by adding the following line at the top of the script, replacing the path with the actual path to this project on your local machine:

```R
setwd("~/Desktop/CPC351-repo/Assignment2")
```

### 3. Execute the Script

Once the data is in place and the working directory is set correctly, you can run the `Assignment2.R` script in your R environment of choice (e.g., RStudio, terminal). The script will print all the results to the console.