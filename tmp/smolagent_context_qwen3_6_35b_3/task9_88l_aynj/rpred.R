library(readr)
# Read and display the first few rows of the output
df <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
print(head(df))
cat("\nNumber of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")
cat("Column names:", paste(names(df), collapse = ", "), "\n")