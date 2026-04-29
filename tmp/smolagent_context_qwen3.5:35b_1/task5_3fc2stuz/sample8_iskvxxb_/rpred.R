# Verify the output file
library(readr)

# Read the output file
final_data <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

# Check structure
cat("=== Output File Verification ===\n")
cat("Number of rows:", nrow(final_data), "\n")
cat("Number of columns:", ncol(final_data), "\n")
cat("Column names:", names(final_data), "\n\n")

# Check variable levels for educ25 and educ32
cat("=== educ25 Summary ===\n")
cat("Unique values:", unique(final_data$educ25), "\n")
cat("Table:\n")
print(table(final_data$educ25, useNA = "ifany"))

cat("\n=== educ32 Summary ===\n")
cat("Unique values:", unique(final_data$educ32), "\n")
cat("Table:\n")
print(table(final_data$educ32, useNA = "ifany"))

cat("\n=== educadtl32 Summary ===\n")
cat("Unique values:", length(unique(final_data$educadtl32)), "categories\n")
cat("Table:\n")
print(table(final_data$educadtl32, useNA = "ifany"))

cat("\n=== educvdtl32 Summary ===\n")
cat("Unique values:", length(unique(final_data$educvdtl32)), "categories\n")
cat("Table:\n")
print(table(final_data$educvdtl32, useNA = "ifany"))

cat("\n=== First 10 rows of data ===\n")
print(head(final_data, 10))

cat("\n=== Verification Complete ===")
