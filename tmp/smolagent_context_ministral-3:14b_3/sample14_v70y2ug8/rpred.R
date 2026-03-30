
# Load required packages
library(dplyr)
library(readr)

# Read the cleaned data
cleaned_data <- read.csv('data/output/cleaned_data.csv')

# Define labels for detailed variables
detailed_labels <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')

# Define labels for harmonized variables
harmonized_labels <- c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')

# Apply labels to detailed variables (ages 14-20)
cleaned_data$hownteen14 <- factor(cleaned_data$hownteen14, levels = -9:13, labels = detailed_labels)
cleaned_data$hownteen15 <- factor(cleaned_data$hownteen15, levels = -9:13, labels = detailed_labels)
cleaned_data$hownteen16 <- factor(cleaned_data$hownteen16, levels = -9:13, labels = detailed_labels)
cleaned_data$hownteen17 <- factor(cleaned_data$hownteen17, levels = -9:13, labels = detailed_labels)
cleaned_data$hownteen18 <- factor(cleaned_data$hownteen18, levels = -9:13, labels = detailed_labels)
cleaned_data$hownteen19 <- factor(cleaned_data$hownteen19, levels = -9:13, labels = detailed_labels)
cleaned_data$hownteen20 <- factor(cleaned_data$hownteen20, levels = -9:13, labels = detailed_labels)

# Apply labels to harmonized variables
cleaned_data$hown14 <- factor(cleaned_data$hown14, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown15 <- factor(cleaned_data$hown15, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown16 <- factor(cleaned_data$hown16, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown17 <- factor(cleaned_data$hown17, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown18 <- factor(cleaned_data$hown18, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown19 <- factor(cleaned_data$hown19, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown20 <- factor(cleaned_data$hown20, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown25 <- factor(cleaned_data$hown25, levels = -9:13, labels = harmonized_labels)
cleaned_data$hown32 <- factor(cleaned_data$hown32, levels = -9:13, labels = harmonized_labels)

# Save the final cleaned dataset
write.csv(cleaned_data, 'data/output/final_cleaned_data.csv', row.names = FALSE)
