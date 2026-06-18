library(dplyr)
library(readr)

# Read and inspect the output
df <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("First 10 rows:\n")
print(head(df, 10))

cat("\nSummary of each variable:\n")
for (col in names(df)) {
  cat(sprintf("\n%s:\n", col))
  cat("Unique values:", length(unique(df[[col]])), "\n")
  cat("Sample values:", paste(head(sort(unique(df[[col]])), 30), collapse = ", "), "\n")
}
