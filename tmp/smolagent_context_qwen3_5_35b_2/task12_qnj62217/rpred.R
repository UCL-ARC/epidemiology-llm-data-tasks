library(readr)

# Read and inspect the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("Output file structure:\n")
str(output)
cat("\nFirst 10 rows:\n")
head(output, 10)
cat("\nMissing value counts:\n")
sapply(output, function(x) sum(is.na(x)))

# Check if all expected variables are present
expected_vars <- c("NSID", "nssec17", "nssec18", "nssec19", "nssec20", "nssec25", "nssec32")
cat("\nVariables present:", paste(names(output), collapse = ", "), "\n")
cat("All expected variables present:", all(expected_vars %in% names(output)), "\n")