library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Check if output file was created
if (file.exists("data/output/cleaned_data.csv")) {
  cat("Output file exists. Checking contents...\n")
  df <- read_csv("data/output/cleaned_data.csv")
  cat("Dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
  cat("Column names:", paste(names(df), collapse = ", "), "\n")
  cat("First few rows:\n")
  print(head(df))
} else {
  cat("Output file not found!\n")
}
