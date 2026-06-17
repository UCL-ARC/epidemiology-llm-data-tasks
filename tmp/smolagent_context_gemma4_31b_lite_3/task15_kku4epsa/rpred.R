library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Merge datasets
cohort_frame <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Process inc25 (from W8DINCB)
# Value labels for W8DINCB:
# -1.0: Not applicable -> -1
# 1.0 to 16.0: Substantive
# Missing (NA) -> -3

cohort_frame <- cohort_frame %>%
  mutate(
    inc25 = as.numeric(W8DINCB),
    inc25 = case_when(
      is.na(inc25) ~ -3,
      inc25 == -1 ~ -1,
      TRUE ~ inc25
    )
  )

# Process inc32 (from W9DINCB)
# Value labels for W9DINCB:
# -1.0: Not applicable -> -1
# 1.0 to 16.0: Substantive
# Missing (NA) -> -3

cohort_frame <- cohort_frame %>%
  mutate(
    inc32 = as.numeric(W9DINCB),
    inc32 = case_when(
      is.na(inc32) ~ -3,
      inc32 == -1 ~ -1,
      TRUE ~ inc32
    )
  )

# Define labels for the income variables
inc_labels <- c(
  "-1" = "Not applicable",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
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
  "16" = "more than 1400"
)

cohort_frame$inc25 <- factor(cohort_frame$inc25, levels = as.numeric(names(inc_labels)), labels = inc_labels)
cohort_frame$inc32 <- factor(cohort_frame$inc32, levels = as.numeric(names(inc_labels)), labels = inc_labels)

# Final selection
final_data <- cohort_frame %>%
  select(NSID, inc25, inc32)

write_csv(final_data, "data/output/cleaned_data.csv")