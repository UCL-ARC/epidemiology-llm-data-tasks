library(readr)

# Read back the output file to verify it contains only the expected columns
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Output file columns:", paste(names(output), collapse = ", "), "\n")
cat("Output file dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("\nFirst 5 rows:\n")
print(head(output, 5))