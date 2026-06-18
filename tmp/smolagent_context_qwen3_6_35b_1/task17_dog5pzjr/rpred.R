library(dplyr)
library(readr)
library(purrr)

# 1. Load files explicitly
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# 2. Merge datasets using full_join by NSID
df <- reduce(list(w1, w2, w3, w4, w9), full_join, by = "NSID")

# 3. Check column names related to IMD
imds <- df %>% select(contains("IMD"))
cat("IMD-related columns:\n")
print(names(imds))
cat("\nFirst few values of each IMD column:\n")
print(head(imds, 10))

# 4. Extract IMDRSCORE columns (they will have .x and .y suffixes from merge)
# w2 has IMDRSCORE (age 15), w3 has IMDRSCORE (age 16)
# After merge, they become IMDRSCORE.x and IMDRSCORE.y
imd15_raw <- df$IMDRSCORE.x
imd16_raw <- df$IMDRSCORE.y
imd32_raw <- df$W9DIMDD

cat("\nimd15_raw stats:\n")
print(table(imd15_raw, useNA = "ifany"))

cat("\nimd16_raw stats:\n")
print(table(imd16_raw, useNA = "ifany"))

cat("\nimd32_raw stats:\n")
print(table(imd32_raw, useNA = "ifany"))

# 5. Apply missing value harmonisation
# Age 15 (imd15): -94 = Insufficient Information → -8, others < 0 → -2, NA → -3
imd15 <- imd15_raw
imd15[imd15 == -94] <- -8
imd15[imd15 < 0 & imd15 != -94] <- -2
imd15[is.na(imd15)] <- -3

# Age 16 (imd16): -94 = Insufficient Information → -8, others < 0 → -2, NA → -3
imd16 <- imd16_raw
imd16[imd16 == -94] <- -8
imd16[imd16 < 0 & imd16 != -94] <- -2
imd16[is.na(imd16)] <- -3

# Age 32 (imd32): -8 = Insufficient information → -8, others < 0 → -2, NA → -3
imd32 <- imd32_raw
imd32[imd32 < 0 & imd32 != -8] <- -2
imd32[is.na(imd32)] <- -3

# 6. Create output dataframe
df_out <- tibble(
  NSID = df$NSID,
  imd15 = imd15,
  imd16 = imd16,
  imd32 = imd32
)

# 7. Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(df_out, "data/output/cleaned_data.csv")

cat("\nSuccessfully created cleaned_data.csv with", nrow(df_out), "rows and", ncol(df_out), "columns.\n")
cat("\nSample of output (first 10 rows):\n")
print(head(df_out, 10))
