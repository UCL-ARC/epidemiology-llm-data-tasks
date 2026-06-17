library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Define file paths
files <- c(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave2 = "data/input/wave_two_lsype_young_person_2020.tab",
  wave3 = "data/input/wave_three_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_self_completion.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

# Load all files
w1 <- read_delim(files["wave1"], delim = "\t", show_col_types = FALSE)
w2 <- read_delim(files["wave2"], delim = "\t", show_col_types = FALSE)
w3 <- read_delim(files["wave3"], delim = "\t", show_col_types = FALSE)
w4 <- read_delim(files["wave4"], delim = "\t", show_col_types = FALSE)
w6 <- read_delim(files["wave6"], delim = "\t", show_col_types = FALSE)
w7 <- read_delim(files["wave7"], delim = "\t", show_col_types = FALSE)
w8 <- read_delim(files["wave8"], delim = "\t", show_col_types = FALSE)
w9 <- read_delim(files["wave9"], delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Helper function to recode missing values
code_missing <- function(x) {
  case_when(
    is.na(x) ~ -3,
    x == -99 ~ -3,
    x == -97 ~ -2,
    x == -96 ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    x == -998 ~ -2,
    x == -997 ~ -2,
    x == -995 ~ -2,
    x == -996 ~ -2,
    x == -999 ~ -2,
    TRUE ~ x
  )
}

# Recode ever_had variables directly from dataframe columns
df$W1_ever <- code_missing(df$W1alceverYP)
df$W2_ever <- code_missing(df$W2alceverYP)
df$W3_ever <- code_missing(df$W3alceverYP)
df$W4_ever <- code_missing(df$W4AlcEverYP)
df$W6_ever <- code_missing(df$W6AlcEverYP)
df$W7_ever <- code_missing(df$W7AlcEverYP)

# For W8 and W9, use AUDIT-1 frequency
# Recode W8 frequency to ever_drink: 1(Never)=0, 2-5=1(has drunk)
df$W8_ever <- case_when(
  df$W8AUDIT1 == 1 ~ 0,  # Never
  df$W8AUDIT1 >= 2 & df$W8AUDIT1 <= 5 ~ 1,  # Has drunk
  TRUE ~ df$W8AUDIT1  # keep missing codes
)

# Recode W9 frequency to ever_drink
df$W9_ever <- case_when(
  df$W9AUDIT1 == 1 ~ 0,  # Never
  df$W9AUDIT1 >= 2 & df$W9AUDIT1 <= 5 ~ 1,  # Has drunk
  TRUE ~ df$W9AUDIT1  # keep missing codes
)

# Derive alcfst: earliest age at which cohort member consumed alcohol
# Using earliest-valid-first rule
# Ages: W1=14, W2=15, W3=16, W4=17, W6=19, W7=20, W8=25, W9=32

alcfst <- rep(99L, nrow(df))  # Default: never drank

# Check each wave in order for first "Yes" (1)
for (i in seq_len(nrow(df))) {
  ever_vals <- c(
    df$W1_ever[i],
    df$W2_ever[i],
    df$W3_ever[i],
    df$W4_ever[i],
    df$W6_ever[i],
    df$W7_ever[i],
    df$W8_ever[i],
    df$W9_ever[i]
  )
  
  ages <- c(14L, 15L, 16L, 17L, 19L, 20L, 25L, 32L)
  
  for (j in seq_along(ever_vals)) {
    val <- ever_vals[j]
    # Check if this is a valid "Yes" response (exactly 1)
    if (!is.na(val) && val == 1) {
      alcfst[i] <- ages[j]
      break
    }
  }
}

df$alcfst <- alcfst

# Keep only NSID and alcfst
df_out <- df %>% select(NSID, alcfst)

# Write to CSV
write_csv(df_out, "data/output/cleaned_data.csv")

# Print summary
cat("Number of rows:", nrow(df_out), "\n")
cat("Number of NSIDs:", length(unique(df_out$NSID)), "\n")
cat("Summary of alcfst:\n")
print(table(df_out$alcfst, useNA = "ifany"))
