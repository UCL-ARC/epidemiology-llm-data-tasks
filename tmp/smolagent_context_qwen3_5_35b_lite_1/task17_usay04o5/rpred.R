library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Check the output file
clean_data <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Number of rows:", nrow(clean_data), "\n")
cat("Number of columns:", ncol(clean_data), "\n")
cat("Columns with IMD:", paste(names(clean_data)[grep("IMD", names(clean_data), ignore.case=TRUE)], collapse=", "), "\n")

# Check for imd15, imd16, imd32
if("imd15" %in% names(clean_data)) {
  cat("imd15 exists. Summary:\n")
  print(summary(clean_data$imd15))
} else {
  cat("imd15 NOT FOUND\n")
}

if("imd16" %in% names(clean_data)) {
  cat("imd16 exists. Summary:\n")
  print(summary(clean_data$imd16))
} else {
  cat("imd16 NOT FOUND\n")
}

if("imd32" %in% names(clean_data)) {
  cat("imd32 exists. Summary:\n")
  print(summary(clean_data$imd32))
} else {
  cat("imd32 NOT FOUND\n")
}