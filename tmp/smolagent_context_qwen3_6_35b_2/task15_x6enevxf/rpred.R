library(readr)

# Read and display the first few rows of the output
df <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Dimensions:", nrow(df), "x", ncol(df), "\n")
cat("\nFirst 10 rows:\n")
print(head(df, 10))
