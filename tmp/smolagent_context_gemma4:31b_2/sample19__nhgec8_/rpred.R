library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
input_dir <- "data/input/"

wave1 <- readr::read_delim(paste0(input_dir, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols())
wave4 <- readr::read_delim(paste0(input_dir, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols())
wave8 <- readr::read_delim(paste0(input_dir, "ns8_2015_derived.tab"), delim = "\t", col_types = readr::cols())
wave9 <- readr::read_delim(paste0(input_dir, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = readr::cols())

merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Harmonisation function
harmonize_bmi <- function(x) {
  res <- case_when(
    is.na(x) ~ -3,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    x > 0 ~ x,
    TRUE ~ -3
  )
  return(res)
}

cleaned_data <- merged_data %>%
  mutate(
    bmi25 = harmonize_bmi(W8DBMI),
    bmi32 = harmonize_bmi(W9DBMI)
  ) %>%
  select(NSID, bmi25, bmi32)

# 5. Variable Type and Labels
# The error 'vec_cast_named' in labelled::labelled occurs because the labels 
# argument should be a named vector where values are the numeric codes and names are the labels,
# OR we use haven::labelled() which is more robust for this purpose.

# Define labels for haven::labelled
# Map: Value -> Label
val_labels <- c(
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Item not applicable" = -1,
  "Not asked/interviewed" = -3,
  "Script error/lost" = -2
)

# Using haven::labelled is the standard way to handle this in the haven/labelled ecosystem
cleaned_data$bmi25 <- haven::labelled(cleaned_data$bmi25, labels = val_labels)
cleaned_data$bmi32 <- haven::labelled(cleaned_data$bmi32, labels = val_labels)

# 8. Output Requirements
write_csv(cleaned_data, "data/output/cleaned_data.csv")