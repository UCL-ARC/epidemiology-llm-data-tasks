library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Dimensions:", dim(output), "\n")
cat("Columns:", names(output), "\n")
cat("First 10 rows:\n")
print(head(output, 10))
