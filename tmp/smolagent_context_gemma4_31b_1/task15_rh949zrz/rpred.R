library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
# The metadata lists 4 files. We must load all to preserve the full cohort frame.
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file2 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file3 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file4 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Merge datasets using full_join on NSID
cohort_frame <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file3, by = "NSID") %>%
  full_join(file4, by = "NSID")

# Process inc25 (from W8DINCB)
# W8DINCB value labels mapping:
# -1.0: Not applicable -> -1
# 1.0 to 16.0: substantive bands
# Convert to numeric first
cohort_frame <- cohort_frame %>%
  mutate(
    W8DINCB_num = as.numeric(W8DINCB),
    inc25 = case_when(
      W8DINCB_num == -1 ~ -1,
      W8DINCB_num >= 1 & W8DINCB_num <= 16 ~ W8DINCB_num,
      TRUE ~ -3
    )
  )

# Process inc32 (from W9DINCB)
# W9DINCB value labels mapping:
# -1.0: Not applicable -> -1
# 1.0 to 16.0: substantive bands
cohort_frame <- cohort_frame %>%
  mutate(
    W9DINCB_num = as.numeric(W9DINCB),
    inc32 = case_when(
      W9DINCB_num == -1 ~ -1,
      W9DINCB_num >= 1 & W9DINCB_num <= 16 ~ W9DINCB_num,
      TRUE ~ -3
    )
  )

# Define labels for the income bands
income_labels <- c(
  "-1" = "Not applicable",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400",
  "-3" = "Not asked at the fieldwork stage / not interviewed"
)

# Apply factor labels
cohort_frame <- cohort_frame %>%
  mutate(
    inc25 = factor(inc25, levels = as.numeric(names(income_labels)), labels = income_labels),
    inc32 = factor(inc32, levels = as.numeric(names(income_labels)), labels = income_labels)
  )

# Select only final variables
final_data <- cohort_frame %>%
  select(NSID, inc25, inc32)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")