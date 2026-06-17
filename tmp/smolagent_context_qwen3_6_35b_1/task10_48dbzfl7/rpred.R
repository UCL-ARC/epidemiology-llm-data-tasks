library(readr)

# Read the output file to verify
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Output file dimensions:", dim(output), "\n")
cat("Column names:", names(output), "\n")

# Show first few rows
print(head(output, 5))

# Show distribution of values for each variable
for (var in names(output)) {
  cat(sprintf("\n%s:\n", var))
  cat("  Unique values:", sort(unique(output[[var]])), "\n")
  cat("  NAs:", sum(is.na(output[[var]])), "\n")
}
