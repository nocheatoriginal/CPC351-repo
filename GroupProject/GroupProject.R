setwd("~/Desktop/HHN/Sem-6/courses/CPC351/CPC351-repo/GroupProject") # Set working directory!!

#
# Imports:
#
library(readxl)

data_dir <- "Data"

safe_name <- function(x) {
  x <- gsub("\\.[^.]+$", "", basename(x))
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (nchar(x) == 0) x <- "dataset"
  x
}

repository <- list()

csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

for (f in csv_files) {
  nm <- safe_name(f)
  repository[[nm]] <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}

xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)

for (f in xlsx_files) {
  base_nm <- safe_name(f)
  sheets <- readxl::excel_sheets(f)
  
  if (length(sheets) == 1) {
    repository[[base_nm]] <- readxl::read_excel(f, sheet = sheets[1])
  } else {
    for (sh in sheets) {
      sh_nm <- gsub("[^A-Za-z0-9_]+", "_", sh)
      sh_nm <- gsub("^_+|_+$", "", sh_nm)
      nm <- paste0(base_nm, "__", sh_nm)
      
      repository[[nm]] <- readxl::read_excel(f, sheet = sh)
    }
  }
}

cat("Loaded datasets:\n")
print(names(repository))

inventory <- data.frame(
  name = names(repository),
  nrows = sapply(repository, nrow),
  ncols = sapply(repository, ncol),
  row.names = NULL
)

inventory[order(as.character(inventory[["name"]])), ]



# ==============================================================================
# CPC351 Project - Uber_Part 2: Regression Model (Fare Prediction)
# ==============================================================================

# 1. Load Required Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(randomForest)
library(caret)

# ==============================================================================
# 2. Data Loading & Cleaning
# ==============================================================================
# Read data
df <- read.csv("uber.csv")

# Check original dimensions
print(paste("Original rows:", nrow(df)))

# Remove missing values
df <- na.omit(df)

# Filter outliers
# 1. The fare must be greater than 0.
df <- df %>% filter(fare_amount > 0)

# 2. The latitude and longitude must be within a reasonable (approximate) range of New York City.
# Exclude points with coordinate 0.
df <- df %>% filter(pickup_longitude != 0 & pickup_latitude != 0 & 
                      dropoff_longitude != 0 & dropoff_latitude != 0)

# Limit the coordinate range (New York City coordinates)
df <- df %>% filter(pickup_latitude > 39 & pickup_latitude < 42 &
                      pickup_longitude > -75 & pickup_longitude < -72 &
                      dropoff_latitude > 39 & dropoff_latitude < 42 &
                      dropoff_longitude > -75 & dropoff_longitude < -72)

print(paste("Cleaned rows:", nrow(df)))

# ==============================================================================
# 3. Feature Engineering
# ==============================================================================

# 3.1 Temporal feature extraction
# Convert pickup_datetime to a time object
df$pickup_datetime <- ymd_hms(df$pickup_datetime)

# Extract specific time components
df$hour <- hour(df$pickup_datetime)
df$day <- day(df$pickup_datetime)
df$month <- month(df$pickup_datetime)
df$year <- year(df$pickup_datetime)
df$weekday <- wday(df$pickup_datetime) # 1=Sunday, etc.

# 3.2 Distance calculation (Haversine Formula)
# Define a function to calculate the distance between two points on the Earth's surface (unit: km).
haversine_dist <- function(lon1, lat1, lon2, lat2) {
  R <- 6367 # Earth's radius (km)
  dlon <- (lon2 - lon1) * pi / 180
  dlat <- (lat2 - lat1) * pi / 180
  a <- sin(dlat/2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  d <- R * c
  return(d)
}

# Calculate distance using the function
df$distance_km <- haversine_dist(df$pickup_longitude, df$pickup_latitude, 
                                 df$dropoff_longitude, df$dropoff_latitude)

# Remove abnormal trips with a distance of 0.
df <- df %>% filter(distance_km > 0)

# ==============================================================================
# 4. Data Splitting
# ==============================================================================
# Select only the relevant features used for modeling
model_data <- df %>% select(fare_amount, distance_km, passenger_count, 
                            hour, day, month, weekday, year)

# Set a random seed to ensure reproducible results.
set.seed(42)

# 80% training set, 20% test set
train_index <- createDataPartition(model_data$fare_amount, p = 0.8, list = FALSE)
train_set <- model_data[train_index, ]
test_set <- model_data[-train_index, ]

print(paste("Training set size:", nrow(train_set)))
print(paste("Test set size:", nrow(test_set)))

# ==============================================================================
# 5. Model Development
# ==============================================================================

# --- Model 1: Linear Regression  ---
print("Training Linear Regression...")
lm_model <- lm(fare_amount ~ ., data = train_set)
summary(lm_model)

# predict
lm_pred <- predict(lm_model, newdata = test_set)

# --- Model 2: Random Forest  ---
# 注意: 数据量较大时，随机森林可能运行较慢。
# Set ntree=50 to speed up the demonstration.
print("Training Random Forest...")
rf_model <- randomForest(fare_amount ~ ., data = train_set, 
                         ntree = 50, importance = TRUE, do.trace = 10)
# predict
rf_pred <- predict(rf_model, newdata = test_set)

# ==============================================================================
# 6. Evaluation
# ==============================================================================

# Define functions to calculate RMSE and R-squared.
eval_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  return(c(RMSE = rmse, R2 = r2))
}

# Evaluate both models
lm_metrics <- eval_metrics(test_set$fare_amount, lm_pred)
rf_metrics <- eval_metrics(test_set$fare_amount, rf_pred)

print("--- Model Evaluation Results ---")
print("Linear Regression:")
print(lm_metrics)
print("Random Forest:")
print(rf_metrics)

# ==============================================================================
# 7. Visualization (Updated)
# ==============================================================================

# Create a data frame for plotting
plot_data <- data.frame(
  Actual = test_set$fare_amount,
  Predicted_LM = lm_pred,
  Predicted_RF = rf_pred
)

# To ensure clear graphics, 500 points were randomly sampled for plotting.
set.seed(123)
subset_plot_data <- plot_data[sample(nrow(plot_data), 500), ]

# --- Plot 1: Linear Regression Results ---
p1 <- ggplot(subset_plot_data, aes(x = Actual, y = Predicted_LM)) +
  geom_point(color = "darkred", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(title = "Linear Regression: Actual vs Predicted Fare",
       subtitle = "Ideally points should be on the dashed line",
       x = "Actual Fare Amount ($)",
       y = "Predicted Fare Amount ($)") +
  theme_minimal()

ggsave("Actual_vs_Predicted_LM.png", plot = p1)
print(p1)

# --- Plot 2: Random Forest Results ---
p2 <- ggplot(subset_plot_data, aes(x = Actual, y = Predicted_RF)) +
  geom_point(color = "steelblue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(title = "Random Forest: Actual vs Predicted Fare",
       subtitle = "Note the tighter cluster around the line compared to Linear Regression",
       x = "Actual Fare Amount ($)",
       y = "Predicted Fare Amount ($)") +
  theme_minimal()

ggsave("Actual_vs_Predicted_RF.png", plot = p2)
print(p2)

# --- Plot 3: Feature Importance ---
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

p3 <- ggplot(importance_df, aes(x = reorder(Feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity", fill = "seagreen") +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest)",
       x = "Features",
       y = "Importance (% Increase MSE)") +
  theme_minimal()

ggsave("Feature_Importance.png", plot = p3)
print(p3)

print("Regression Analusis Completed!")




# ==============================================================================
# CPC351 Project - Uber_Part 2: Clustering Model (Hotspot Detection)
# ==============================================================================

# 1. Load Required Libraries
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(cluster)) install.packages("cluster")

library(dplyr)
library(ggplot2)
library(cluster)

# ==============================================================================
# 2. Data Loading & Cleaning
# ==============================================================================
# Read data
df <- read.csv("uber.csv")

# Basic cleaning
df <- na.omit(df)
df <- df %>% filter(pickup_longitude != 0 & pickup_latitude != 0)

# Focusing on the New York City area (removing obvious outliers for better clustering results).
# Latitude: 40.60 to 40.90, Longitude: -74.05 to -73.70
df <- df %>% filter(pickup_latitude > 40.60 & pickup_latitude < 40.90 &
                      pickup_longitude > -74.05 & pickup_longitude < -73.70)

# Extract features for clustering (latitude and longitude only)
cluster_data <- df %>% select(pickup_longitude, pickup_latitude)


# ==============================================================================
# 3. Determine Optimal K (Elbow Method)
# ==============================================================================
# To save time, we only calculate the first 10,000 points to draw the Elbow graph.
set.seed(123)
sample_data <- cluster_data[sample(nrow(cluster_data), 10000), ]

# Calculate WCSS
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(sample_data, centers = i)$withinss)
}

# Drawing the Elbow Method diagram
elbow_df <- data.frame(k = 1:10, wcss = wcss)

p1 <- ggplot(elbow_df, aes(x = k, y = wcss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method for Optimal K",
       x = "Number of Clusters (k)",
       y = "WCSS") +
  theme_minimal()

ggsave("Elbow_Method.png", plot = p1)
print(p1)

# ==============================================================================
# 4. Model Development (K-Means)
# ==============================================================================
# Set K = 5 
set.seed(42)
k_final <- 5
kmeans_result <- kmeans(cluster_data, centers = k_final, nstart = 20)

# Add the clustering results to the original data.
df$cluster <- as.factor(kmeans_result$cluster)

# Obtain cluster Centroids
centroids <- as.data.frame(kmeans_result$centers)
centroids$cluster <- as.factor(1:k_final)
print("Cluster Centers:")
print(centroids)

# ==============================================================================
# 5. Visualization
# ==============================================================================

# To ensure clear visualization, 2000 points were randomly sampled.
set.seed(123)
plot_sample <- df[sample(nrow(df), 2000), ]

p2 <- ggplot() +
  # draw points
  geom_point(data = plot_sample, aes(x = pickup_longitude, y = pickup_latitude, color = cluster), 
             alpha = 0.6, size = 1) +
  # Draw the Centroids
  geom_point(data = centroids, aes(x = pickup_longitude, y = pickup_latitude), 
             color = "black", shape = 4, size = 5, stroke = 2) +
  labs(title = "Uber Pickup Hotspots (K-Means Clustering)",
       subtitle = "Red 'X' marks the center of high-demand areas",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

ggsave("Cluster_Map.png", plot = p2)
print(p2)

print("Clustering Analysis Completed.")
