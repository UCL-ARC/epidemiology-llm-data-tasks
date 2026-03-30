# Verify the output file exists and check its contents
library(readr)

# Check if file exists
if (file.exists("data/output/cleaned_data.csv")) {
  cat("Output file created successfully!\n")
  
  # Read and display structure
  data <- read_csv("data/output/cleaned_data.csv")
  cat("\nFile structure:\n")
  cat("Rows:", nrow(data), "\n")
  cat("Columns:", ncol(data), "\n")
  cat("\nColumn names:", paste(names(data), collapse = ", "), "\n")
  
  # Show first few rows
  cat("\nFirst 10 rows:\n")
  print(head(data, 10))
  
  # Show summary of eth variable
  cat("\nEthnicity distribution (eth):\n")
  print(table(data$eth, useNA = "ifany"))
} else {
  cat("ERROR: Output file not found!\n")
}
