# Verify the output file exists and check its contents
library(readr)

# Check if file exists
if (file.exists("data/output/cleaned_data.csv")) {
  # Read and display structure
  output <- read_csv("data/output/cleaned_data.csv")
  cat("File created successfully!\n")
  cat("Number of rows:", nrow(output), "\n")
  cat("Number of columns:", ncol(output), "\n")
  cat("Column names:\n")
  print(names(output))
  cat("\nFirst few rows:\n")
  print(head(output))
} else {
  cat("ERROR: Output file not found!\n")
}