library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define files to load from metadata
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load files and merge
data_list <- lapply(files, function(f) {
  readr::read_delim(paste0('data/input/', f), delim = '\t')
})

# Merge all datasets by NSID
full_frame <- data_list[[1]]
if (length(data_list) > 1) {
  for (i in 2:length(data_list)) {
    full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
  }
}

# Process variables based on metadata
# W8DINCB (Wave 8/Age 25) and W9DINCB (Wave 9/Age 32)
# Mapping: -1.0 -> -1 (Not applicable), NA -> -3 (Not asked)

final_data <- full_frame %>%
  mutate(
    inc25 = case_when(
      is.na(W8DINCB) ~ -3,
      W8DINCB == -1.0 ~ -1,
      TRUE ~ W8DINCB
    ),
    inc32 = case_when(
      is.na(W9DINCB) ~ -3,
      W9DINCB == -1.0 ~ -1,
      TRUE ~ W9DINCB
    )
  ) %>%
  select(NSID, inc25, inc32)

# Define factor levels for the income bands based on metadata
income_labels <- c(
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

# Apply labels to factors
final_data <- final_data %>%
  mutate(
    inc25 = factor(inc25, levels = as.numeric(names(income_labels)), labels = income_labels),
    inc32 = factor(inc32, levels = as.numeric(names(income_labels)), labels = income_labels)
  )

# Write output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')