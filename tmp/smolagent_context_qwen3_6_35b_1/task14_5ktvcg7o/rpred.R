library(readr)
library(dplyr)

df <- read_csv("data/output/cleaned_data.csv")
cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")
cat("Column names:", paste(names(df), collapse = ", "), "\n")
cat("\nFirst 5 rows:\n")
print(as.data.frame(df[1:5, ]))

cat("\nFactor levels for hownteen14:\n")
print(levels(df$hownteen14))

cat("\nUnique values for hownteen14:\n")
print(sort(unique(df$hownteen14)))

cat("\nUnique values for hown14:\n")
print(sort(unique(df$hown14)))

cat("\nUnique values for hown25:\n")
print(sort(unique(df$hown25)))

cat("\nUnique values for hown32:\n")
print(sort(unique(df$hown32)))
