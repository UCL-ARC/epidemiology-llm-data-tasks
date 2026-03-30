# Verify the output file
library(readr)

# Read and check the output
output <- read_csv("data/output/cleaned_data.csv")
cat("Output verification:\n")
cat("Rows:", nrow(output), "\n")
cat("Columns:", paste(names(output), collapse = ", "), "\n")
cat("\nFirst 10 rows:\n")
print(head(output, 10))
cat("\nMissing value summary:\n")
for (col in names(output)) {
  if (col != "NSID") {
    vals <- output[[col]]
    cat(col, ": ", sum(vals == -9, na.rm=TRUE), " refusals (-9), ",
        sum(vals == -8, na.rm=TRUE), " DK (-8), ",
        sum(vals == -3, na.rm=TRUE), " not asked (-3), ",
        sum(vals == -2, na.rm=TRUE), " script error (-2), ",
        sum(vals == -1, na.rm=TRUE), " N/A (-1)\n", sep="")
  }
}