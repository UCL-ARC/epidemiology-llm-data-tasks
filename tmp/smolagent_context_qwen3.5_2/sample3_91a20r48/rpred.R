# Verify the output file exists and check its contents
library(readr)

# Check if file exists
if (file.exists("data/output/cleaned_data.csv")) {
  cat("Output file created successfully!\n")
  
  # Read and display first few rows
  output <- read_csv("data/output/cleaned_data.csv")
  cat("\nFirst 10 rows of cleaned data:\n")
  print(head(output, 10))
  
  cat("\n\nSummary of lang variable:\n")
  print(table(output$lang, useNA = "ifany"))
  
  cat("\n\nDimensions:", dim(output), "\n")
  cat("Column names:", paste(names(output), collapse = ", "), "\n")
} else {
  cat("ERROR: Output file not found!\n")
}