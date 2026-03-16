# Check the output file
library(readr)
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
print(head(output))
cat("\nColumn names:\n")
print(names(output))
cat("\nNumber of rows:", nrow(output), "\n")
cat("\nNumber of columns:", ncol(output), "\n")