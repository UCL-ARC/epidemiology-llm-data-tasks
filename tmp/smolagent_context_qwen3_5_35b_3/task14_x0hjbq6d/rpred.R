library(readr)

# Read the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Column names:\n")
cat(names(output), "\n")
cat("\nRow count:", nrow(output), "\n")
cat("\nFirst 5 rows:\n")
print(head(output, 5))
