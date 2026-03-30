# Verify the output file
output <- read.csv("data/output/cleaned_data.csv")
cat("Output file verification:\n")
cat("Dimensions:", dim(output), "\n")
cat("Column names:", paste(names(output), collapse=", "), "\n")
cat("\nFirst few rows:\n")
print(head(output))
cat("\nSummary of eth_consolidated:\n")
print(table(output$eth_consolidated, useNA = "ifany"))