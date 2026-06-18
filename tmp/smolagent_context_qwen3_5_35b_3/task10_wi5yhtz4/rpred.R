library(readr)
library(dplyr)

# Read the output file and check it
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Output file dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("Column names:", paste(names(output), collapse = ", "), "\n")
cat("\nFirst 5 rows:\n")
print(head(output, 5))
cat("\nVariable types:\n")
str(output)
