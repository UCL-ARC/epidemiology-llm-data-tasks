library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Paths
input_dir  <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

# List of files to load
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "ns9_2022_derived_variables.tab"
)

# Load all files as character columns
raw_data <- lapply(files, function(f) {
  read_delim(file.path(input_dir, f), delim = "\t", col_types = cols(.default = "c"))
})
names(raw_data) <- files

# Merge on NSID
merged <- reduce(raw_data, full_join, by = "NSID")

# Convert numeric fields
merged <- merged %>%
  mutate(
    IMDRSCORE.x = as.numeric(IMDRSCORE.x),   # Wave 2 (age 15)
    IMDRSCORE.y = as.numeric(IMDRSCORE.y),   # Wave 3 (age 16)
    W9DIMDD    = as.numeric(W9DIMDD)        # Wave 9 (age 32 decile)
  )

# Derive IMD for age 15
imd15 <- ifelse(
  is.na(merged$IMDRSCORE.x),
  -3,
  ifelse(
    merged$IMDRSCORE.x == -94,
    -8,
    ifelse(merged$IMDRSCORE.x < 0, -2, merged$IMDRSCORE.x)
  )
)

# Derive IMD for age 16
imd16 <- ifelse(
  is.na(merged$IMDRSCORE.y),
  -3,
  ifelse(
    merged$IMDRSCORE.y == -94,
    -8,
    ifelse(merged$IMDRSCORE.y < 0, -2, merged$IMDRSCORE.y)
  )
)

# Derive IMD for age 32 (decile) – factor with labels
imd32_raw <- merged$W9DIMDD
imd32_levels <- c(-8, -3, -2, -1, -9, -7, 1:10)
imd32_labels <- c(
  "Insufficient information",
  "Not asked at fieldwork stage / not interviewed",
  "Schedule not applicable / script error / information lost",
  "Item not applicable",
  "Refusal",
  "Prefer not to say",
  "Most deprived decile",
  "Second decile",
  "Third decile",
  "Fourth decile",
  "Fifth decile",
  "Sixth decile",
  "Seventh decile",
  "Eighth decile",
  "Ninth decile",
  "Least deprived decile"
)
imd32 <- factor(imd32_raw, levels = imd32_levels, labels = imd32_labels)

# Assemble final dataset
final_df <- merged %>%
  mutate(imd15 = imd15, imd16 = imd16, imd32 = imd32) %>%
  select(NSID, imd15, imd16, imd32)

# Write to CSV (NA values written as empty strings)
write_csv(final_df, output_file, na = "")