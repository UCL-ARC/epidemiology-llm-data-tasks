library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
wave1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = "c"))
wave2 <- readr::read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = "c"))
wave4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = "c"))
wave8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols(.default = "c"))
wave9 <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols(.default = "c"))

# Merge datasets using full_join by NSID
full_df <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# 2. Identify and Prepare Source Variables
# Mapping based on metadata
# Wave 1: W1ethnic2YP
# Wave 2: W2ethnicYP
# Wave 4: w4ethnic2YP
# Wave 8: W8DETHN15
# Wave 9: W9DETHN15

# Function to harmonise missing values based on labels
# -9 Refusal, -8 Don't know/Insufficient, -7 Prefer not to say, -3 Not asked, -2 Schedule error, -1 Not applicable
harmonise_missing <- function(val, mapping) {
  # Convert to numeric first as they were read as character
  num_val <- as.numeric(val)
  
  res <- case_when(
    is.na(num_val) ~ -3,
    # Use the specific labels provided in metadata to map to standard codes
    # The provided mapping should be a named vector where name=source_val, value=standard_code
    # We check if num_val exists in the mapping
    TRUE ~ {
      # This is a bit tricky in case_when, let's use a helper
      val_str <- as.character(num_val)
      if (val_str %in% names(mapping)) {
        as.numeric(mapping[[val_str]])
      } else {
        num_val
      }
    }
  )
  return(res)
}

# Since case_when logic above is slightly flawed for vectorization, using a custom map
map_missing <- function(vec, labels_map) {
  num_vec <- as.numeric(vec)
  out <- num_vec
  for (src in names(labels_map)) {
    out[num_vec == as.numeric(src)] <- labels_map[[src]]
  }
  out[is.na(out)] <- -3
  return(out)
}

# Define mappings based on metadata labels
# Wave 1: -999:lost(-2), -94:insufficient(-8), -92:refused(-9), -91:N/A(-1), -1:DK(-8)
map1 <- c("-999.0" = -2, "-94.0" = -8, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8)

# Wave 2: -998:missed(-3), -997:error(-2), -995:unexplained(-2), -99:not interviewed(-3), -92:refused(-9), -91:N/A(-1), -1:DK(-8)
map2 <- c("-998.0" = -3, "-997.0" = -2, "-995.0" = -2, "-99.0" = -3, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8)

# Wave 4: -94:insufficient(-8), -1:DK(-8)
map4 <- c("-94.0" = -8, "-1.0" = -8)

# Wave 8: -9:refused(-9), -8:insufficient(-8), -1:N/A(-1)
map8 <- c("-9.0" = -9, "-8.0" = -8, "-1.0" = -1)

# Wave 9: -8:insufficient(-8)
map9 <- c("-8.0" = -8)

# Process variables
full_df <- full_df %>%
  mutate(
    eth1 = map_missing(W1ethnic2YP, map1),
    eth2 = map_missing(W2ethnicYP, map2),
    eth4 = map_missing(w4ethnic2YP, map4),
    eth8 = map_missing(W8DETHN15, map8),
    eth9 = map_missing(W9DETHN15, map9)
  )

# 3. Consolidation: Earliest-valid-first
# Substantive values are 1-16. Missing are < 0.

consolidate_ethnicity <- function(...) {
  dots <- list(...)
  # Find first value that is >= 1
  # We use a loop over the columns for each row
  res <- apply(as.data.frame(dots), 1, function(row) {
    vals <- as.numeric(row)
    valid_idx <- which(vals >= 1)[1]
    if (!is.na(valid_idx)) {
      return(vals[valid_idx])
    } else {
      # Fallback to missing codes: use the first non-NA missing code encountered
      # or simply the first value if all are missing codes
      first_non_na <- which(!is.na(vals))[1]
      if (!is.na(first_non_na)) return(vals[first_non_na])
      return(-3)
    }
  })
  return(res)
}

full_df$eth <- consolidate_ethnicity(full_df$eth1, full_df$eth2, full_df$eth4, full_df$eth8, full_df$eth9)

# 4. Factor Labels
# The categories 1-16 are consistent across waves (White-British, ..., Any other)
# We use the labels from Wave 1 as the reference
ethnicity_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

full_df$eth <- factor(full_df$eth, levels = as.numeric(names(ethnicity_labels)), labels = ethnicity_labels)

# Final Output
final_output <- full_df %>%
  select(NSID, eth)

readr::write_csv(final_output, "data/output/cleaned_data.csv")