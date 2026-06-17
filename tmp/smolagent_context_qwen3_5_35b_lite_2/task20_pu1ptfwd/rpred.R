library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
head(output, 10)
cat("\nTotal rows:", nrow(output), "\n")
cat("Total columns:", ncol(output), "\n")