library(readr)
library(dplyr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Output file structure:\n")
print(str(output))
cat("\nFirst 10 rows:\n")
print(head(output, 10))
cat("\nColumn names:\n")
print(colnames(output))
cat("\nNumber of rows:", nrow(output), "\n")
cat("\nMissing values per column:\n")
print(colSums(is.na(output)))