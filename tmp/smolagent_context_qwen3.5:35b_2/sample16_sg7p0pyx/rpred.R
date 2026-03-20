# Check the output file
library(readr)
library(dplyr)

# Read the cleaned data
cleaned_data <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Display structure and sample
cat("Structure of cleaned data:\n")
str(cleaned_data)

cat("\nFirst 10 rows:\n")
print(head(cleaned_data, 10))

cat("\nSummary of each variable:\n")
summary(cleaned_data)