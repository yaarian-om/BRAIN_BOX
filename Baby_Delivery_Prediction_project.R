# Installing Library
## install.packages("corrplot")
library(knitr)
opts_knit$set(output.format = "pdf")
# Importing Files
data <- read.csv("C:/Users/User/Desktop/Assignments/DATA SCIENCE/Data-Science-Project/Dataset_midterm.csv", header = TRUE, sep = ",")
# data <- read.csv("C:/Users/ASUS/Desktop/Data-Science-Project/Dataset_midterm.csv",header = TRUE, sep = ",")
data # Decision Taken that is => This is a labelled dataset
# Create a copy of the dataset so that if anything goes wrong we do not import the dataset again and again.
data_frame <- data
data_frame


# Data Preparation
##      |--------- Data Quality
###                      |--------- After reading the descriptions of all the attributes, we can come to a conclusion that is Caesarian is the target variable and all the other variables are the independent variables.
###                                 But the Target variable is in the form of categorical data(Numerical). Finally We can say that the [quality is quite good].
##      |--------- Data Exploration
###                      |--------- At first, Let's know the [dimensions] of the dataset
dim(data)
### So, there are 80 rows and 8 columns in the dataset
###                      |--------- let's know the [data types] of the variables of the dataset
str(data)
### So, Blood, Delivery time, Heart, Caesarian are the attributes which is categorical. All of them are in integer format (except Weight and Blood), including the target variable(Categorical).
###                      |--------- Let's see the [Column names] of the dataset
names(data)
### So, we can see that there are No spaces(The only issue with Weight(Kg)) in the column names.
###                      |--------- Let's see the [first 6 rows] of the dataset
head(data)
###                      |--------- Let's know the [summary](Missing-Values and outliers) of the dataset
summary(data)
### There are some missing values in the dataset. Missing values are in  Age, Weight, Delivery_Time and Caesarian attributes.
###                      |--------- [Counting] how many missing values are avaiable in total in the whole dataset
sum(is.na(data))
### Okay so there are total 13 missing values in the dataset.
##      |--------- Column Renaming
colnames(data_frame)[3] <- "Weight"
## Change the 3rd column name from Weight(Kg) to Weight
names(data_frame)
##      |--------- Data Transformation = Categorical Encoding
data_frame$Blood <- factor(data$Blood, levels = c("low", "normal", "high"), labels = c(0, 1, 2))
head(data_frame)
##      |--------- Data type conversion
data_frame$Age <- as.numeric(data_frame$Age)
data_frame$Delivery_number <- as.numeric(data_frame$Delivery_number)
data_frame$Delivery_time <- as.numeric(data_frame$Delivery_time) # Here data was not coming as character. So, that's why we have first converted it to character and then in numaric
data_frame$Blood <- as.numeric(data_frame$Blood)
data_frame$Heart <- as.numeric(data_frame$Heart)
data_frame$Caesarian <- as.numeric(data_frame$Caesarian)
str(data_frame)
summary(data_frame)
## All the attributes are in numerical format now except the ID
##      |--------- Handling [Missing Values]
###                      |--------- [Remove the missing values]
data_omitted_missing_values <- na.omit(data_frame) # So, we have removed the missing values from the dataset
summary(data_omitted_missing_values)
sum(is.na(data_omitted_missing_values))
dim(data_omitted_missing_values) # Checking some rows are deleted or not
### So, we can see that there are 67 rows and 8 columns in the dataset
###                      |--------- Use [Mean] to fill the missing values (Age, Weight, Delivery_number, Delivery_time, Blood, Caesarian)
data_frame_mean <- data_frame
data_frame_mean$Age[is.na(data_frame$Age)] <- mean(data_frame$Age, na.rm = TRUE)
data_frame_mean$Weight[is.na(data_frame$Weight)] <- mean(data_frame$Weight, na.rm = TRUE)
data_frame_mean$Delivery_number[is.na(data_frame$Delivery_number)] <- mean(data_frame$Delivery_number, na.rm = TRUE)
data_frame_mean$Delivery_time[is.na(data_frame$Delivery_time)] <- mean(data_frame$Delivery_time, na.rm = TRUE)
data_frame_mean$Blood[is.na(data_frame$Blood)] <- mean(data_frame$Blood, na.rm = TRUE)
data_frame_mean$Caesarian[is.na(data_frame$Caesarian)] <- mean(data_frame$Caesarian, na.rm = TRUE)
summary(data_frame_mean)
### So, we can see that there is NO values still available in delivery_time attribute.
sum(is.na(data_frame_mean))
### Returns 0. So no missing values in the entire dataset
###                      |--------- Use [Median] to fill the missing values (Age, Weight, Delivery_number, Delivery_time, Blood, Caesarian)
data_frame_median <- data_frame
data_frame_median$Age[is.na(data_frame$Age)] <- median(data_frame$Age, na.rm = TRUE)
data_frame_median$Weight[is.na(data_frame$Weight)] <- median(data_frame$Weight, na.rm = TRUE)
data_frame_median$Delivery_number[is.na(data_frame$Delivery_number)] <- median(data_frame$Delivery_number, na.rm = TRUE)
data_frame_median$Delivery_time[is.na(data_frame$Delivery_time)] <- median(data_frame$Delivery_time, na.rm = TRUE)
data_frame_median$Blood[is.na(data_frame$Blood)] <- median(data_frame$Blood, na.rm = TRUE)
data_frame_median$Caesarian[is.na(data_frame$Caesarian)] <- median(data_frame$Caesarian, na.rm = TRUE)
summary(data_frame_median)
### So, we can see that there is NO values still available in delivery_time attribute
sum(is.na(data_frame_median))
### Returns 0. So no missing values in the entire dataset
###                      |--------- Use [Mode] to fill the missing values (Age, Weight, Delivery_number, Delivery_time, Blood, Caesarian)
####                                                      |--------- We can not use mode value because we have converted the dataset into numerical data.
####                                                                 So, we can not use mode value to fill the missing values.
####                                                                 And moreover before that the dataset was in integer format. So we can not use mode value
####                                                                 to fill the missing values.

# Univariate Exploration
##      |--------- Find the [Range] of each column of the dataset
apply(data_frame, 2, range, na.rm = TRUE)
## Here na.rm = TRUE means that we are ignoring the missing values and apply the function on the remaining values
##      |--------- Find the [Standard Deviation] of each column of the dataset
apply(data_frame, 2, sd, na.rm = TRUE)
##      |--------- Find the [Variance] of each column of the dataset
apply(data_frame, 2, var, na.rm = TRUE)
##      |--------- Find the [Co-Relations] of each column of the dataset
cor(data_frame_median)
## Here we have used the data_frame_median because it has no missing values otherwise co-relations get affected by the missing values
## install.packages("corrplot")
library(corrplot)
## Create a correlation matrix
cor_matrix <- cor(data_frame, use = "pairwise.complete.obs")
## Plot the correlation matrix
corrplot(cor_matrix, method = "circle")
