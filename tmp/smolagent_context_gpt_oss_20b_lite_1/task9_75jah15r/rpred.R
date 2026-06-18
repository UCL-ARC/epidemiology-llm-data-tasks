# Load required packages
library(readr)
library(dplyr)
library(labelled)

# Helper function to map wave-specific missing codes to standard codes
map_missing <- function(x) {
  case_when(
    x == -999 ~ -2,  # schedule not applicable / script error / info lost
    x == -99 ~ -3,   # not interviewed
    x == -98 ~ -3,   # not present
    x == -94 ~ -8,   # insufficient information
    x == -92 ~ -9,   # refused
    x == -91 ~ -1,   # not applicable
    x == -1 ~ -7,    # don\'t know / prefer not to say
    TRUE ~ x
  )
}

# Helper to collapse detailed categories into 5-level NVQ scheme
collapse_nvq <- function(x) {
  case_when(
    # Keep missing codes unchanged
    x %in% c(-9, -8, -7, -3, -2, -1) ~ x,
    # NVQ categories
    x == 4 ~ 1,  # HNC/HND/NVQ4
    x == 9 ~ 2,  # City and guilds part III, NVQ3
    x == 14 ~ 3, # City and guilds part II, NVQ2
    x == 17 ~ 4, # City and guilds part I, NVQ1
    # All other detailed levels treated as “Other”
    TRUE ~ 5
  )
}

# Load wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols())
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols())
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols())

# Select relevant columns and rename for consistency
wave1_sel <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_sel <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_sel <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Map missing codes for each wave
wave1_sel <- wave1_sel %>% mutate(
  mom_dtl_w1 = map_missing(W1hiqualmum),
  dad_dtl_w1 = map_missing(W1hiqualdad)
)
wave2_sel <- wave2_sel %>% mutate(
  mom_dtl_w2 = map_missing(W2hiqualmum),
  dad_dtl_w2 = map_missing(W2hiqualdad)
)
wave4_sel <- wave4_sel %>% mutate(
  mom_dtl_w4 = map_missing(w4hiqualmum),
  dad_dtl_w4 = map_missing(w4hiqualdad)
)

# Merge all waves by NSID
merged <- wave1_sel %>%
  full_join(wave2_sel, by = "NSID") %>%
  full_join(wave4_sel, by = "NSID")

# Create consolidated detailed parental education variables (earliest valid first)
merged <- merged %>% mutate(
  educdtlma = coalesce(mom_dtl_w1, mom_dtl_w2, mom_dtl_w4),
  educdtlpa = coalesce(dad_dtl_w1, dad_dtl_w2, dad_dtl_w4)
)

# Create consolidated NVQ collapsed variables
merged <- merged %>% mutate(
  educma = collapse_nvq(educdtlma),
  educpa = collapse_nvq(educdtlpa)
)

# Replace any remaining NA (i.e., no valid response across all waves) with -3 (not asked)
merged <- merged %>% mutate(
  educdtlma = ifelse(is.na(educdtlma), -3, educdtlma),
  educdtlpa = ifelse(is.na(educdtlpa), -3, educdtlpa),
  educma   = ifelse(is.na(educma),   -3, educma),
  educpa   = ifelse(is.na(educpa),   -3, educpa)
)

# Keep only final derived variables and ID
final_data <- merged %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write CSV to output
write_csv(final_data, "data/output/cleaned_data.csv")