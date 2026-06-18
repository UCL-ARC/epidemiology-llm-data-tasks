library(readr)

# Read the output to verify
out <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
print(paste("Rows:", nrow(out)))
print(paste("Columns:", paste(names(out), collapse = ", ")))

# Show first 10 rows
print(head(out, 10))

# Check for any issues
print(paste("Any NAs in NSID:", sum(is.na(out$NSID))))
print(paste("Any empty NSID:", sum(out$NSID == "")))