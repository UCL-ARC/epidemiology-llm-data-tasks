# Verify the output file exists and check its structure
library(readr)

# Read the cleaned data
cleaned_data <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check the structure
str(cleaned_data)

# Check variable names
colnames(cleaned_data)

# Check first few rows
head(cleaned_data)