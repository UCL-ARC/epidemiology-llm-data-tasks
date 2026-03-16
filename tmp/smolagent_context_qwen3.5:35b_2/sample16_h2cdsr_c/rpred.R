# Verify the output file exists and check its contents
library(readr)

# Read the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Output file contents:\n")
print(head(output, 10))

cat("\nColumn types:\n")
print(str(output))

cat("\nNumber of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")
cat("Column names:", names(output), "\n")
