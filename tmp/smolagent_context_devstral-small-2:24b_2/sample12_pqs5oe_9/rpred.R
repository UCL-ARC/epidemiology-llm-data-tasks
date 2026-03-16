library(haven)
library(dplyr)

# Read the cleaned data file
cleaned_data <- read.csv("data/output/cleaned_data.csv", nrows = 10)

# Check the structure of the data
cat("Data types:\n")
print(sapply(cleaned_data, class))

# Check unique values in one of the NS-SEC variables
cat("\nUnique values in nssec17:\n")
print(unique(cleaned_data$nssec17))

# Check if the file has the correct number of rows
cat("\nNumber of rows in output file:", nrow(cleaned_data), "\n")

# Verify all required columns are present
required_cols <- c("NSID", "nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")
cat("\nAll required columns present:", all(required_cols %in% names(cleaned_data)), "\n")