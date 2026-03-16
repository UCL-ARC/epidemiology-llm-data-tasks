# Verify the output file
library(readr)
library(labelled)

cleaned_data <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Column names:\n")
print(names(cleaned_data))

cat("\nNumber of observations:", nrow(cleaned_data), "\n")
cat("Number of variables:", ncol(cleaned_data), "\n")

cat("\nFirst 5 rows:\n")
print(head(cleaned_data, 5))

cat("\nValue labels for nssecma14:\n")
print(labelled::val_labels(cleaned_data$nssecma14))

cat("\nSummary statistics for nssecma14:\n")
print(table(cleaned_data$nssecma14, useNA = "ifany"))