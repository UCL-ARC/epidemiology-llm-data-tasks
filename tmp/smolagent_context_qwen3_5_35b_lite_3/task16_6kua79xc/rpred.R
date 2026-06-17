library(readr)

# Read the output to verify
cleaned <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)

cat("=== Output File Structure ===\n")
str(cleaned)

cat("\n=== First 10 rows ===\n")
head(cleaned, 10)

cat("\n=== Summary of banded variables ===\n")
cat("Banded 14 - unique values:", unique(cleaned$w1_banded_income14), "\n")
cat("Banded 15 - unique values:", unique(cleaned$w2_banded_income15), "\n")
cat("Banded 16 - unique values:", unique(cleaned$w3_banded_income16), "\n")
cat("Banded 17 - unique values:", unique(cleaned$w4_banded_income17), "\n")

cat("\n=== Missing value codes present ===\n")
cat("Banded 14 missing codes:", sort(unique(cleaned$w1_banded_income14[!is.na(cleaned$w1_banded_income14) & cleaned$w1_banded_income14 <= 0])), "\n")
cat("Banded 15 missing codes:", sort(unique(cleaned$w2_banded_income15[!is.na(cleaned$w2_banded_income15) & cleaned$w2_banded_income15 <= 0])), "\n")
cat("Banded 16 missing codes:", sort(unique(cleaned$w3_banded_income16[!is.na(cleaned$w3_banded_income16) & cleaned$w3_banded_income16 <= 0])), "\n")
cat("Banded 17 missing codes:", sort(unique(cleaned$w4_banded_income17[!is.na(cleaned$w4_banded_income17) & cleaned$w4_banded_income17 <= 0])), "\n")

cat("\n=== Summary of continuous variables ===\n")
cat("Cont 14 - unique values:", sort(unique(cleaned$w1_cont_income14)), "\n")
cat("Cont 15 - unique values:", sort(unique(cleaned$w2_cont_income15)), "\n")