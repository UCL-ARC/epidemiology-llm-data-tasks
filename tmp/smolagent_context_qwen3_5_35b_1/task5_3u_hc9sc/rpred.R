library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
print(head(output, 20))
cat("\nColumn names:", names(output), "\n")
cat("\nNumber of rows:", nrow(output), "\n")
cat("\nNumber of columns:", ncol(output), "\n")

# Check for any issues with NA values
cat("\nSummary of NA values per column:\n")
print(sapply(output, function(x) sum(is.na(x))))