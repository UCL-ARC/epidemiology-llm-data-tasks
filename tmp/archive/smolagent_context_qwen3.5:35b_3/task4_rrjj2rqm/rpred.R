# Verify the output file exists and check its contents
library(readr)

# Read the cleaned data
cleaned_data <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Display structure and first few rows
str(cleaned_data)
head(cleaned_data)