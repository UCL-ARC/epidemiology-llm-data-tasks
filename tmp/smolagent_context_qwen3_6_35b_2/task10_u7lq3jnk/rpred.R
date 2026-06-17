library(dplyr)
library(readr)

# Read the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check the structure
glimpse(output)

# Check first few rows
print(head(output, 10))

# Check for any issues with the variables
cat("\nSummary of ecoact17:\n")
cat("Unique values:", paste(unique(output$ecoact17), collapse=", "), "\n")

cat("\nSummary of ecoact25:\n")
cat("Unique values:", paste(unique(output$ecoact25), collapse=", "), "\n")

cat("\nSummary of ecoactadu25:\n")
cat("Unique values:", paste(unique(output$ecoactadu25), collapse=", "), "\n")

# Check counts
cat("\nRow count:", nrow(output), "\n")
cat("NSID count:", length(output$NSID), "\n")