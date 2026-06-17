library(dplyr)
library(readr)
library(haven)
library(labelled)
library(tidyr)
library(purrr)

# Load all files from data/input/
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID using full_join
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w3, by = "NSID")
df <- full_join(df, w4, by = "NSID")

# Function to map employment status to standard codes
map_employment <- function(x) {
  case_when(
    x == -999 ~ -2,
    x == -996 ~ -2,
    x == -99 ~ -3,
    x == -98 ~ -3,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x >= 1 & x <= 9 ~ x,
    TRUE ~ NA_real_
  )
}

# Apply mapping to each variable and create output variables
df <- df %>%
  mutate(
    ecoactma14 = map_employment(W1empsmum),
    ecoactpa14 = map_employment(W1empsdad),
    ecoactma15 = map_employment(W2empsmum),
    ecoactpa15 = map_employment(W2empsdad),
    ecoactma16 = map_employment(W3empsmum),
    ecoactpa16 = map_employment(W3empsdad),
    ecoactma17 = map_employment(w4empsmum),
    ecoactpa17 = map_employment(w4empsdad)
  )

# Define value labels for the output variables
valid_labels <- c(
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other"
)

missing_labels <- c(
  "-2" = "Schedule not applicable",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-8" = "Don't know / insufficient information",
  "-9" = "Refusal"
)

all_labels <- c(valid_labels, missing_labels)

# Create the named numeric vector for haven::labelled
# names = labels, values = numeric codes
label_vec <- setNames(as.numeric(names(all_labels)), all_labels)

# Apply labels to each output variable
ecoact_vars <- c("ecoactma14", "ecoactpa14", "ecoactma15", "ecoactpa15",
                 "ecoactma16", "ecoactpa16", "ecoactma17", "ecoactpa17")

for (var in ecoact_vars) {
  df[[var]] <- haven::labelled(df[[var]], label_vec)
}

# Keep only NSID and the derived variables
df_out <- df %>% select(NSID, all_of(ecoact_vars))

# Write output
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Done. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_out), "\n")
cat("Columns:", paste(names(df_out), collapse = ", "), "\n")

# Quick summary
for (var in ecoact_vars) {
  cat("\n", var, ":\n")
  cat("  Unique values:", length(unique(df_out[[var]])), "\n")
  cat("  NAs (R):", sum(is.na(df_out[[var]])), "\n")
}
