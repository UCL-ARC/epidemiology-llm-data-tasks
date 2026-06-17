library(readr)
# Read and display the first 20 rows of the output
df <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
print(head(df, 20))
cat("\nTotal rows:", nrow(df), "\n")
cat("\nColumns:", names(df), "\n")
cat("\nUnique alcfst values:", sort(unique(df$alcfst)), "\n")