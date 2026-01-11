setwd("~/Desktop/HHN/Sem-6/courses/CPC351/CPC351-repo/GroupProject") # Set working directory!!


# ==============================================================================
# CPC351 Project - Part 2
# ==============================================================================

# 1. Load Required Libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(randomForest)
library(caret)
library(ranger)
library(cluster)

set.seed(123)

# ==============================================================================
# 1) Load crab age prediction data
# ==============================================================================
df <- read.csv("Data/CrabAgePrediction.csv", stringsAsFactors = FALSE)
names(df) <- make.names(names(df))  # ensures clean names like Shucked.Weight

# Ensure correct types
df$Sex <- factor(df$Sex)      # typically F, M, I
df$Age <- as.numeric(df$Age)

cat("Rows:", nrow(df), "Cols:", ncol(df), "\n")
cat("Sex levels:", paste(levels(df$Sex), collapse = ", "), "\n")
cat("Missing total:", sum(is.na(df)), "\n\n")

# ==============================================================================
# 2) Feature engineering
# ==============================================================================
df$BodyVolumeProxy <- df$Length * df$Diameter * df$Height

safe_log <- function(x) log(pmax(x, 1e-9))
df$LogWeight        <- safe_log(df$Weight)
df$LogShuckedWeight <- safe_log(df$Shucked.Weight)
df$LogVisceraWeight <- safe_log(df$Viscera.Weight)
df$LogShellWeight   <- safe_log(df$Shell.Weight)

# ==============================================================================
# 3) Train/Test split (80/20)
# ==============================================================================
n <- nrow(df)
test_idx <- sample(seq_len(n), size = round(0.2 * n))
train <- df[-test_idx, ]
test  <- df[test_idx, ]

# Validation split inside training (for stacking weights)
val_idx   <- sample(seq_len(nrow(train)), size = round(0.2 * nrow(train)))
train_sub <- train[-val_idx, ]
val_sub   <- train[val_idx, ]

# ==============================================================================
# 4) Metrics helpers
# ==============================================================================
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

report_metrics <- function(name, y, yhat) {
  cat("\n===", name, "===\n")
  cat("RMSE:", round(rmse(y, yhat), 3), "\n")
  cat("MAE :", round(mae(y, yhat), 3), "\n")
  cat("R^2 :", round(r2(y, yhat), 3), "\n")
}

# ==============================================================================
# PART A) AGE PREDICTION (Combined / Ensemble)
# ==============================================================================

age_features <- c(
  "Sex", "Length", "Diameter", "Height",
  "Weight", "Shucked.Weight", "Viscera.Weight", "Shell.Weight",
  "BodyVolumeProxy",
  "LogWeight", "LogShuckedWeight", "LogVisceraWeight", "LogShellWeight"
)

age_formula <- as.formula(paste("Age ~", paste(age_features, collapse = " + ")))

# A1) Linear model
lm_age <- lm(age_formula, data = train_sub)
val_pred_lm <- predict(lm_age, newdata = val_sub)

# A2) Random Forest regression (fast)
rf_age <- ranger(
  formula = age_formula,
  data = train_sub,
  num.trees = 400,
  mtry = max(2, floor(sqrt(length(age_features)))),
  min.node.size = 5,
  sample.fraction = 0.8,
  importance = "permutation",
  respect.unordered.factors = "order",
  num.threads = max(1, parallel::detectCores() - 1),
  seed = 123
)
val_pred_rf <- predict(rf_age, data = val_sub)$predictions

# A3) Stacking model (learns best combination of LM + RF on validation)
stack_df <- data.frame(Age = val_sub$Age, pred_lm = val_pred_lm, pred_rf = val_pred_rf)
stack_model <- lm(Age ~ pred_lm + pred_rf, data = stack_df)

cat("\nStacking coefficients (validation):\n")
print(coef(stack_model))

# Retrain base models on full training set
lm_age_full <- lm(age_formula, data = train)
rf_age_full <- ranger(
  formula = age_formula,
  data = train,
  num.trees = 400,
  mtry = max(2, floor(sqrt(length(age_features)))),
  min.node.size = 5,
  sample.fraction = 0.8,
  importance = "permutation",
  respect.unordered.factors = "order",
  num.threads = max(1, parallel::detectCores() - 1),
  seed = 123
)

# Predict on test
test_pred_lm <- predict(lm_age_full, newdata = test)
test_pred_rf <- predict(rf_age_full, data = test)$predictions

b <- coef(stack_model)
test_pred_ensemble <- b[1] + b["pred_lm"] * test_pred_lm + b["pred_rf"] * test_pred_rf

# Metrics
report_metrics("Age Model - Linear Regression (test)", test$Age, test_pred_lm)
report_metrics("Age Model - Random Forest (test)", test$Age, test_pred_rf)
report_metrics("Age Model - ENSEMBLE (stacked) (test)", test$Age, test_pred_ensemble)

# Plot: Actual vs Predicted (Ensemble)
plot(test$Age, test_pred_ensemble,
     xlab = "Actual Age", ylab = "Predicted Age (Ensemble)",
     main = "Ensemble Age Prediction: Actual vs Predicted (Test)",
     pch = 19, col = rgb(0,0,0,0.25))
abline(0, 1, col = "red", lwd = 2)

# Feature importance for age (RF)
age_imp <- sort(ranger::importance(rf_age_full), decreasing = TRUE)
cat("\nTop 10 Age Feature Importances (RF):\n")
print(round(age_imp[1:min(10, length(age_imp))], 4))

barplot(age_imp[1:min(15, length(age_imp))],
        las = 2, cex.names = 0.75,
        main = "Age Prediction - RF Permutation Importance (Top 15)")

# ==============================================================================
# PART B) SEX PREDICTION (Classification)
# ==============================================================================

sex_features <- c(
  "Length", "Diameter", "Height",
  "Weight", "Shucked.Weight", "Viscera.Weight", "Shell.Weight",
  "BodyVolumeProxy",
  "LogWeight", "LogShuckedWeight", "LogVisceraWeight", "LogShellWeight"
)

sex_formula <- as.formula(paste("Sex ~", paste(sex_features, collapse = " + ")))

sex_model <- ranger(
  formula = sex_formula,
  data = train,
  num.trees = 500,
  mtry = max(2, floor(sqrt(length(sex_features)))),
  min.node.size = 5,
  sample.fraction = 0.8,
  probability = TRUE,
  importance = "permutation",
  respect.unordered.factors = "order",
  num.threads = max(1, parallel::detectCores() - 1),
  seed = 123
)

sex_prob <- predict(sex_model, data = test)$predictions
sex_pred <- colnames(sex_prob)[max.col(sex_prob, ties.method = "first")]
sex_pred <- factor(sex_pred, levels = levels(test$Sex))

cat("\n=== Sex Prediction (Multiclass) ===\n")
cat("Accuracy:", round(mean(sex_pred == test$Sex), 3), "\n")
cat("Confusion Matrix:\n")
print(table(Predicted = sex_pred, Actual = test$Sex))

sex_imp <- sort(ranger::importance(sex_model), decreasing = TRUE)
cat("\nTop 10 Sex Feature Importances:\n")
print(round(sex_imp[1:min(10, length(sex_imp))], 4))

barplot(sex_imp[1:min(15, length(sex_imp))],
        las = 2, cex.names = 0.75,
        main = "Sex Prediction - RF Permutation Importance (Top 15)")

# ==============================================================================
# PART C) CLUSTERING (K-means + Silhouette + PCA plot)
# ==============================================================================

cluster_features <- c("Length","Diameter","Height","Weight","Shucked.Weight","Viscera.Weight","Shell.Weight")
X <- df[, cluster_features]
X_scaled <- scale(X)

# C1) Choose k using silhouette (k=2..8)
k_values <- 2:8
sil_scores <- numeric(length(k_values))
wss <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  km <- kmeans(X_scaled, centers = k, nstart = 10, iter.max = 50)
  wss[i] <- km$tot.withinss
  sil <- silhouette(km$cluster, dist(X_scaled))
  sil_scores[i] <- mean(sil[, 3])
}

best_k <- k_values[which.max(sil_scores)]
cat("\n=== Clustering ===\n")
cat("Best k by silhouette:", best_k, "\n")

# Elbow plot
plot(k_values, wss, type = "b",
     xlab = "k", ylab = "Total Within-Cluster SS",
     main = "Elbow Method (K-means)")

# Silhouette plot
plot(k_values, sil_scores, type = "b",
     xlab = "k", ylab = "Average Silhouette",
     main = "Silhouette Scores (K-means)")

# C2) Fit final k-means
set.seed(123)
km_final <- kmeans(X_scaled, centers = best_k, nstart = 25, iter.max = 100)
df$Cluster <- factor(km_final$cluster)

# C3) PCA for visualization
pca <- prcomp(X_scaled, center = TRUE, scale. = FALSE)
pc <- pca$x[, 1:2]

# OPTIONAL: show top contributors to PC1/PC2 (so you can justify labeling)
loadings <- pca$rotation[, 1:2]
pc1_top <- sort(abs(loadings[,1]), decreasing = TRUE)
pc2_top <- sort(abs(loadings[,2]), decreasing = TRUE)
cat("\nTop contributors to PC1:\n")
print(round(pc1_top[1:3], 3))
cat("\nTop contributors to PC2:\n")
print(round(pc2_top[1:3], 3))

# C4) Rename cluster labels to something meaningful
#     We label clusters based on mean Weight (size/mass)
cluster_summary <- aggregate(
  df[, c("Weight", "Length", "Diameter", "Height", "Age")],
  list(Cluster = df$Cluster),
  mean
)

cat("\nCluster summary (means):\n")
print(cluster_summary)

small_cluster <- as.character(cluster_summary$Cluster[which.min(cluster_summary$Weight)])
large_cluster <- as.character(cluster_summary$Cluster[which.max(cluster_summary$Weight)])

df$ClusterLabel <- as.character(df$Cluster)
df$ClusterLabel[df$ClusterLabel == small_cluster] <- "Smaller / Lighter"
df$ClusterLabel[df$ClusterLabel == large_cluster] <- "Larger / Heavier"
df$ClusterLabel <- factor(df$ClusterLabel, levels = c("Smaller / Lighter", "Larger / Heavier"))

cat("\nSex distribution by cluster label:\n")
print(prop.table(table(df$ClusterLabel, df$Sex), margin = 1))

cat("\nMean Age by cluster label:\n")
print(aggregate(df$Age, list(Cluster = df$ClusterLabel), mean))

# C5) Plot PCA with meaningful labels + centers
plot(pc[,1], pc[,2],
     col = as.numeric(df$ClusterLabel),
     pch = 19,
     xlab = "PC1 (Overall size / mass)",
     ylab = "PC2 (Secondary shape variation)",
     main = paste("K-means Clusters (k =", best_k, ") in PCA space"),
     cex = 0.7)

# centers on PCA space
centers_pc <- aggregate(pc, list(Cluster = df$ClusterLabel), mean)
points(centers_pc$PC1, centers_pc$PC2, pch = 4, cex = 2.2, lwd = 3)

legend("topright",
       legend = levels(df$ClusterLabel),
       col = 1:length(levels(df$ClusterLabel)),
       pch = 19,
       title = "Cluster Type",
       cex = 0.9)



# ==============================================================================
# 2. Data Loading & Cleaning
# ==============================================================================
# Read data
df <- read.csv("Data/uber.csv")

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

print("Regression Analysis Completed!")


# ==============================================================================
# Uber dataset clustering model
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
df <- read.csv("Data/uber.csv")

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
