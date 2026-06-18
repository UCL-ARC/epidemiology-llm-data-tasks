library(readr)
library(dplyr)

# Load source files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols())
ns8  <- read_delim("data/input/ns8_2015_derived.tab",   delim = "\t", col_types = cols())
ns9  <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols())

# Merge on NSID
merged <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(ns8,  by = "NSID") %>%
  full_join(ns9,  by = "NSID")

# Helper to harmonise income bands
process_income <- function(x) {
  ifelse(is.na(x), -3L, ifelse(x == -1.0, -1L, as.integer(x)))
}

# Create income band variables
merged <- merged %>%
  mutate(
    inc25 = process_income(W8DINCB),
    inc32 = process_income(W9DINCB)
  )

# Keep only final variables
final <- merged %>% select(NSID, inc25, inc32)

# Write output
write_csv(final, "data/output/cleaned_data.csv")