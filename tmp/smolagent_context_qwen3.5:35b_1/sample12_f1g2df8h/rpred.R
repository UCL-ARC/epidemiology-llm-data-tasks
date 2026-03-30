# Verify the output file
library(readr)
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
head(output)
cat("\nFile dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("\nColumn names:", paste(names(output), collapse = ", "), "\n")