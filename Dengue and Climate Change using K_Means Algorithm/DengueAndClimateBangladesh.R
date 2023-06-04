#! Project Hierarchy
# ================================================================================ # nolint
# DengueAndClimateBangladesh
#           ├── Install Libraries
#           ├── Load Libraries
#           ├── Load Dataset
#           ├── Data Preprocessing
#                       ├── Dataset Dimension
#                       ├── Dataset Structure(Data Types & Value-Glimpse)
#                       ├── Dataset Summery
#           ├── Feature Selection
#                       ├── Find-out the Optimal K by Elbow Method
#           ├── K Means Clustering Model Training
#                       ├── Assigning Severity Levels to Incidents (Clusters)
#           ├── Result(Show Extracted Information's)
#                       ├── Add the severity levels to the original dataset
#                       ├── Plot the Clustering Result by Clusplot
#                       ├── Plot the Clustering Result by Autoplot
# ================================================================================ # nolint

#*  1. Install Libraries
# ------------------------------
# install.packages("tidyverse")
# install.packages("cluster")
# install.packages("caret")
# install.packages("dplyr")
# ------------------------------

#*  2. Load Libraries
# ------------------------------
library(tidyverse)
library(cluster)
library(caret)
library(dplyr)
# ------------------------------

#*  3. Load Dataset
# ------------------------------
dengue_data <- read.csv("DengueAndClimateBangladesh.csv")
df = dengue_data
head(df)
# ------------------------------

#*  4. Data Preprocessing
# ==============================
#  4.1 Dataset Dimension
dim(df)
# ------------------------------
#  4.2 Dataset Structure (Data Types & Value-Glimpse)
str(df)
# ------------------------------
#  4.3 Dataset Summery(Mean, Median, Min, 1st-Quadrant, 3rd-Quadrant)
summary(df)
cat("Total Missing Values = ",sum(is.na(df)))
# ------------------------------
# ==============================

#*  5. Feature Selection
# ==============================
# Select features with correlation coefficient >= 0.3
df_corr_pos <- cor(df[, -1])
highly_correlated_pos <- findCorrelation(df_corr_pos, cutoff = 0.3)

# Select features with correlation coefficient <= -0.9
df_corr_neg <- cor(df[, -1])
highly_correlated_neg <- findCorrelation(abs(df_corr_neg) >= 0.9, cutoff = 0)

# Combine selected features
dengue_features <- df[, c(highly_correlated_pos+1, highly_correlated_neg+1)]
dengue_std <- scale(dengue_features) # Standardize the features to ensure equal weight in clustering # nolint

# ------------------------------
#  5.1 Find-out the Optimal K by Elbow Method
# ------------------------------
# Calculate WSS for k=1 to 10
# Calculate WSS for k=1 to 10
wss <- sapply(1:10, function(k){
  kmeans(dengue_std, centers=k)$tot.withinss
})

# Plot the elbow curve
plot(1:10, wss, type="b", xlab="Number of clusters (K)", ylab="WSS")

# Determine the optimal number of clusters (K)
k.optimal <- which.min(wss)
cat("Optimal value of K = ",k.optimal)
# ------------------------------
# ==============================

#*  6. K Means Clustering Model Training
# ==============================
set.seed(123) # set a seed for reproducibility
kmeans_model <- kmeans(dengue_std, centers=k.optimal)
# ------------------------------
#  6.1 Assigning Severity Levels to Incidents (Clusters)
# ------------------------------
severity_levels <- case_when(
  kmeans_model$cluster == 1 ~ "Very Low",
  kmeans_model$cluster == 2 ~ "Low",
  kmeans_model$cluster == 3 ~ "Lower-Medium",
  kmeans_model$cluster == 4 ~ "Medium",
  kmeans_model$cluster == 5 ~ "Upper-Medium",
  kmeans_model$cluster == 6 ~ "High",
  kmeans_model$cluster == 7 ~ "Higher-High",
  kmeans_model$cluster == 8 ~ "Very High",
  kmeans_model$cluster == 9 ~ "Critical",
  kmeans_model$cluster == 10 ~ "Life-Threatening",
  TRUE ~ NA_character_
)
# ------------------------------
# ==============================

#  7*. Result(Show Extracted Information's)
# ==============================
#  7.1 Add the severity levels to the original dataset
dengue_data$Severity <- severity_levels
tail(dengue_data, n=5)
# ------------------------------
#  7.2 Plot the Clustering Result by Clusplot
clusplot(dengue_std, kmeans_model$cluster, color = TRUE, labels = 2,
         lines = 0, main = "Cluster plot of dengue dataset (K-means clustering)") 
# ------------------------------
#  7.3 Plot the Clustering Result by Autoplot
# ------------------------------
autoplot(kmeans_model,dengue_std, frame = TRUE)
# ------------------------------
# ==============================


