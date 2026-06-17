library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("Dimensions:", nrow(output), "rows,", ncol(output), "columns\n")
cat("\nColumn names:\n")
print(names(output))
cat("\nFirst 10 rows:\n")
print(head(output, 10))
cat("\nMissing value counts per variable:\n")
for (col in names(output)) {
  if (col == "NSID") next
  vals <- output[[col]]
  count_neg3 <- sum(vals == -3, na.rm = TRUE)
  count_neg8 <- sum(vals == -8, na.rm = TRUE)
  count_neg9 <- sum(vals == -9, na.rm = TRUE)
  count_neg1 <- sum(vals == -1, na.rm = TRUE)
  count_neg2 <- sum(vals == -2, na.rm = TRUE)
  count_na <- sum(is.na(vals))
  cat(sprintf("%s: -3=%d, -8=%d, -9=%d, -1=%d, -2=%d, NA=%d\n", col, count_neg3, count_neg8, count_neg9, count_neg1, count_neg2, count_na))
}
cat("\nUnique values in incwhh14 (first 30):\n")
print(sort(unique(output$incwhh14), na.last = TRUE))
cat("\nUnique values in incwhhcnt14 (first 30):\n")
print(sort(unique(output$incwhhcnt14), na.last = TRUE))