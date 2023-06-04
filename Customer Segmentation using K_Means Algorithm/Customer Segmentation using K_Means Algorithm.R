# Clustering & Prediction Using K-Means Clustering
# ==================================================
## Libraries Needed
### install.packages("cluster")
library(cluster) # Used to get the K Means Cluster Algorithm/Model
library(corrplot) # Used to Plot Co-Relationship of the Attributes
### Libraries Loaded Successfully

## Load the Dataset
training_data <- read.csv("Train.csv")
head(training_data)
data <- training_data
### Dataset Loaded Successfully

## Data Pre-processing
### Dataset Dimension
dim(data)
####  Dataset has 8068 Columns and 11 Attributes

### Dataset Structure (Data Types & Value-Glimpse)
str(data)
#### Dataset has 2 int & 2 num, 7 Categorical character type Attributes

### Converting Data to Categorical Variables using the factor() Function for Better Analysis
data$Gender <- factor(data$Gender , levels = c("Male", "Female"), labels = c(1,0), exclude = NULL)
data$Ever_Married <- factor(data$Ever_Married , levels = c("Yes", "No"), labels = c(1,0), exclude = NULL)
data$Graduated <- factor(data$Graduated , levels = c("Yes", "No"), labels = c(1,0), exclude = NULL)
data$Profession <- factor(data$Profession , levels = c("Engineer", "Doctor","Executive","Artist","Lawyer","Healthcare","Entertainment","Marketing","Homemaker"), labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8), exclude = NULL)
data$Spending_Score <- factor(data$Spending_Score, levels = c("Low", "Average", "High"), labels = c(0, 1, 2), exclude = NULL)
data$Var_1 <- factor(data$Var_1, levels = c("Cat_1", "Cat_2", "Cat_3","Cat_4","Cat_5","Cat_6","Cat_7"), labels = c(0, 1, 2, 3, 4, 5, 6), exclude = NULL)
data$Segmentation <- factor(data$Segmentation, levels = c("A", "B", "C","D"), labels = c(0, 1, 2, 3), exclude = NULL)
head(data)
#### Converted to the Categorical Variable Successfully

### Convert Data Types to Numaric
data$ID <- as.numeric(data$ID)
data$Gender <- as.numeric(as.character(data$Gender))
data$Ever_Married <- as.numeric(as.character(data$Ever_Married))
data$Age <- as.numeric(data$Age)
data$Graduated <- as.numeric(as.character(data$Graduated))
data$Profession <- as.numeric(as.character(data$Profession))
data$Work_Experience <- as.numeric(data$Work_Experience)
data$Spending_Score <- as.numeric(as.character(data$Spending_Score))
data$Family_Size <- as.numeric(data$Family_Size)
data$Var_1 <- as.numeric(as.character(data$Var_1))
data$Segmentation <- as.numeric(as.character(data$Segmentation))
corr_data = data
str(data)
#### All Data Types Converted to Numeric Successfully

### Dataset Attributes Name
names(data)
#### No issue with attributes name. Everything is fine

### Dataset Summery(Mean, Median, Min, 1st-Quadrant, 3rd-Quadrant)
#### Summery is used to find-out, which columns has Missing Values (How many) & Outliers(There is a big difference between MEAN & MEDIAN respect to the other columns differences)
summary(data)
cat("Total Missing Values = ",sum(is.na(data)))
#### Missing values found in Ever_Married(140), Graduated(78), Profession(124), Work_Experience(829), Family_Size(335), Var_1(76)
#### AND
#### Based on the differences between Mean and Median, the Attribute -> Age, Work_Experience & Family_Size could have OUTLIERS

### Fix the Missing values By Dropping them
data <- na.omit(data)
cat("Total Missing Values = ",sum(is.na(data)))
#### Missing Values has been Removed Successfully

## Data Co-Relation & Data Visualization
cor_matrix <- cor(corr_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "circle")
corrplot(cor_matrix, method = "number")
### Selected Features : [ Ever_Married, Age, Graduated, Profession, Spending_Score, Family_Size ] to predict the Segmentation

## Split the dataset into training (80%) and testing sets (20%)
set.seed(123) # for reproducibility
train_indexes <- sample(nrow(data), round(0.8 * nrow(data))) # Splitting 80% For Training Purpose and 20% for Testing Purpose
train_data <- data[train_indexes, ]
test_data <- data[-train_indexes, ]
### Split the Dataset into Training set and Testing set Successfully

## Choose the attributes for training the model
features = c("Ever_Married", "Age", "Graduated", "Profession", "Spending_Score", "Family_Size")
X_train = train_data[, features]
y_train = train_data$Segmentation
### Attributes has been chosen Successfully

## Train the K-Means Clustering Model
kmeans_model <- kmeans(X_train, centers = 2)
### Model has been Trained Successfully

## Show The Trained Model Description
kmeans_model
print("Centers of the Attributes for each Cluster")
kmeans_model$centers
cat("Total Number of Iteration = ", kmeans_model$iter)

