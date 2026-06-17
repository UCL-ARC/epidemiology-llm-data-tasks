library(dplyr)
library(readr)
library(haven)
library(labelled)

# Define file paths
files <- list(
  "wave_one_lsype_young_person_2020.tab" = "wave1",
  "wave_four_lsype_young_person_2020.tab" = "wave4",
  "ns8_2015_derived.tab" = "wave8",
  "ns9_2022_derived_variables.tab" = "wave9"
)

# Load all files
datasets <- list()
for (fname in names(files)) {
  path <- file.path("data/input", fname)
  datasets[[fname]] <- read_delim(path, delim = "\t", show_col_types = FALSE)
}

# Merge all datasets by NSID using full_join
cleaned <- datasets[["wave_one_lsype_young_person_2020.tab"]]
for (fname in names(datasets)[-1]) {
  cleaned <- full_join(cleaned, datasets[[fname]], by = "NSID")
}

# Create inc25 from W8DINCB (Wave 8, Age 25)
cleaned <- cleaned %>%
  mutate(
    inc25 = case_when(
      is.na(W8DINCB) ~ -3,
      W8DINCB == -1 ~ -2,
      W8DINCB >= 1 & W8DINCB <= 16 ~ as.double(W8DINCB),
      TRUE ~ -3
    )
  )

# Create inc32 from W9DINCB (Wave 9, Age 32)
cleaned <- cleaned %>%
  mutate(
    inc32 = case_when(
      is.na(W9DINCB) ~ -3,
      W9DINCB == -1 ~ -2,
      W9DINCB >= 1 & W9DINCB <= 16 ~ as.double(W9DINCB),
      TRUE ~ -3
    )
  )

# Add variable labels
cleaned <- cleaned %>%
  set_variable_labels(
    inc25 = "Banded weekly income at age 25",
    inc32 = "Banded weekly income of cohort member and partner at age 32"
  )

# Keep only NSID and final derived variables
cleaned <- cleaned %>%
  select(NSID, inc25, inc32)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Write to CSV
write_csv(cleaned, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
print(head(cleaned, 10))