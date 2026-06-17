library(readr)

# Check the output file
output <- read_csv("data/output/cleaned_data.csv")
cat("Output file dimensions:", nrow(output), ncol(output), "\n")
cat("Output columns:", paste(names(output), collapse = ", "), "\n")
cat("\nFirst 10 rows:\n")
print(head(output, 10))
cat("\nLang variable distribution:\n")
print(table(output$lang, useNA = "ifany"))
cat("\nLang variable labels:\n")
print(attr(output$lang, "labels"))
cat("\nLang variable label attribute:\n")
print(attr(output$lang, "label"))