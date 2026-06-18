library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
# Note: The metadata mentions 4 files, we load all to preserve the cohort frame
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols(.default = "c"))
file9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols(.default = "c"))

# Merge datasets
merged_data <- file1 %>%
  full_join(file4, by = "NSID") %>%
  full_join(file8, by = "NSID") %>%
  full_join(file9, by = "NSID")

# Process inc25 (from W8DINCB)
# Metadata: -1.0 = Not applicable. Valid: 1-16.
# Mapping: -1 -> -1 (Item not applicable)
# NAs -> -3 (Not asked/not interviewed)
merged_data <- merged_data %>%
  mutate(
    W8DINCB_num = as.numeric(W8DINCB),
    inc25 = case_when(
      is.na(W8DINCB_num) ~ -3,
      W8DINCB_num == -1 ~ -1,
      W8DINCB_num >= 1 & W8DINCB_num <= 16 ~ W8DINCB_num,
      TRUE ~ -3
    )
  )

# Process inc32 (from W9DINCB)
# Metadata: -1.0 = Not applicable. Valid: 1-16.
merged_data <- merged_data %>%
  mutate(
    W9DINCB_num = as.numeric(W9DINCB),
    inc32 = case_when(
      is.na(W9DINCB_num) ~ -3,
      W9DINCB_num == -1 ~ -1,
      W9DINCB_num >= 1 & W9DINCB_num <= 16 ~ W9DINCB_num,
      TRUE ~ -3
    )
  )

# Define labels for income bands
income_labels <- c(
  "1" = "less than 25", "2" = "25 to 50", "3" = "50 to 90", "4" = "90 to 140",
  "5" = "140 to 240", "6" = "240 to 300", "7" = "300 to 350", "8" = "350 to 400",
  "9" = "400 to 500", "10" = "500 to 600", "11" = "600 to 700", "12" = "700 to 800",
  "13" = "800 to 900", "14" = "900 to 1200", "15" = "1200 to 1400", "16" = "more than 1400",
  "-1" = "Not applicable", "-3" = "Not asked at the fieldwork stage / not interviewed"
)

merged_data$inc25 <- factor(merged_data$inc25, levels = as.numeric(names(income_labels)), labels = income_labels)
merged_data$inc32 <- factor(merged_data$inc32, levels = as.numeric(names(income_labels)), labels = income_labels)

# Select final variables
final_data <- merged_data %>%
  select(NSID, inc25, inc32)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")