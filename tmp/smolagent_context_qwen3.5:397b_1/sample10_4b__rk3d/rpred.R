# Verify the output file
library(readr)

# Read and display the first few rows and structure
data <- read_csv("data/output/cleaned_data.csv")

cat("=== Output File Summary ===\n")
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")
cat("\nColumn names:\n")
print(names(data))
cat("\nFirst 10 rows:\n")
print(head(data, 10))
cat("\nStructure:\n")
str(data)
