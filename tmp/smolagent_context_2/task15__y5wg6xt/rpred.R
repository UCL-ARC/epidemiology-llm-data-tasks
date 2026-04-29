library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define paths based on metadata
file_w1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file_w4 <- "data/input/wave_four_lsype_young_person_2020.tab"
file_w8 <- "data/input/ns8_2015_derived.tab"
file_w9 <- "data/input/ns9_2022_derived_variables.tab"

# Load files
data_w1 <- read_delim(file_w1, delim = "\t", col_types = readr::cols(.default = "c"))
data_w4 <- read_delim(file_w4, delim = "\t", col_types = readr::cols(.default = "c"))
data_w8 <- read_delim(file_w8, delim = "\t", col_types = readr::cols(.default = "c"))
data_w9 <- read_delim(file_w9, delim = "\t", col_types = readr::cols(.default = "c"))

# Merge datasets
df <- data_w1 %>%
  full_join(data_w4, by = "NSID") %>%
  full_join(data_w8, by = "NSID") %>%
  full_join(data_w9, by = "NSID")

# Convert target variables to numeric for processing
df <- df %>%
  mutate(
    W8DINCB = as.numeric(W8DINCB),
    W9DINCB = as.numeric(W9DINCB)
  )

# 6 & 7. Standard Missing-Value Codes and Harmonisation
# Mapping for income bands:
# -1.0 is 'Not applicable' -> maps to -1
# NA/Null -> -3

process_income <- function(x) {
  x[is.na(x)] <- -3
  x[x == -1] <- -1
  return(x)
}

df <- df %>%
  mutate(
    income_w8 = process_income(W8DINCB),
    income_w9 = process_income(W9DINCB)
  )

# 10. Labels and Data Types
# Band labels from metadata
band_labels <- c(
  "1" = "less than 25", "2" = "25 to 50", "3" = "50 to 90", "4" = "90 to 140",
  "5" = "140 to 240", "6" = "240 to 300", "7" = "300 to 350", "8" = "350 to 400",
  "9" = "400 to 500", "10" = "500 to 600", "11" = "600 to 700", "12" = "700 to 800",
  "13" = "800 to 900", "14" = "900 to 1200", "15" = "1200 to 1400", "16" = "more than 1400",
  "-1" = "Not applicable", "-3" = "Not asked at the fieldwork stage / not interviewed"
)

df <- df %>%
  mutate(
    income_w8 = factor(income_w8, levels = as.numeric(names(band_labels)), labels = band_labels),
    income_w9 = factor(income_w9, levels = as.numeric(names(band_labels)), labels = band_labels)
  )

# 12. Output Requirements
final_df <- df %>%
  select(NSID, income_w8, income_w9)

write_csv(final_df, "data/output/cleaned_data.csv")