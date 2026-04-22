# Verify the output file exists and check its contents
library(readr)

# Read the output file to verify
cleaned_data <- read_csv("data/output/cleaned_data.csv")

# Display structure and summary
cat("=== Output File Verification ===\n")
cat("File: data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(cleaned_data), "\n")
cat("Number of columns:", ncol(cleaned_data), "\n")
cat("\nColumn names:\n")
print(names(cleaned_data))
cat("\nFirst 10 rows:\n")
print(head(cleaned_data, 10))
cat("\nSummary statistics:\n")
print(summary(cleaned_data))