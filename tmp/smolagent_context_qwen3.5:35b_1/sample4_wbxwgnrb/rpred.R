library(readr)

# Check the output file
output_file <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Display summary
print(paste("Number of rows:", nrow(output_file)))
print(paste("Number of columns:", ncol(output_file)))
print("Column names:")
print(names(output_file))
print("First few rows:")
print(head(output_file))
print("Summary of sori variable:")
print(summary(output_file$sori))