library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
load_data <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols())
}

data1 <- load_data('wave_one_lsype_young_person_2020.tab')
data4 <- load_data('wave_four_lsype_young_person_2020.tab')
data6 <- load_data('wave_six_lsype_young_person_2020.tab')
data8 <- load_data('ns8_2015_derived.tab')
data9 <- load_data('ns9_2022_derived_variables.tab')

merged_df <- data1 %>%
  full_join(data4, by = 'NSID') %>%
  full_join(data6, by = 'NSID') %>%
  full_join(data8, by = 'NSID') %>%
  full_join(data9, by = 'NSID')

# 2. Vectorized Mapping Function
# Since case_when and mapping with vectors can be tricky in R, 
# we use a helper that handles the vectorization explicitly.
map_vector <- function(vec, mapping) {
  # Initialize result with the original values
  res <- vec
  # Map the values found in the mapping keys
  for (code in names(mapping)) {
    res[vec == as.numeric(code)] <- mapping[[code]]
  }
  # Handle NAs as -3
  res[is.na(res)] <- -3
  return(res)
}

# 3. Processing Variables

# --- Age 19 (Sweep 6) ---
map_w6 <- c(
  '-997' = -2,
  '-97' = -7,
  '-92' = -9,
  '-91' = -1,
  '-1' = -8
)

merged_df <- merged_df %>%
  mutate(
    partnr19 = map_vector(W6MarStatYP, map_w6)
  )

# --- Age 25 (Wave 8) ---
map_w8 <- c(
  '-9' = -9,
  '-8' = -8,
  '-1' = -1
)

merged_df <- merged_df %>%
  mutate(
    partnradu25 = map_vector(W8DMARSTAT, map_w8),
    partnr25 = case_when(
      partnradu25 == 1 | partnradu25 == 8 ~ 1,
      partnradu25 == 2 | partnradu25 == 6 ~ 2,
      partnradu25 == 3 | partnradu25 == 7 ~ 3,
      partnradu25 == 4 ~ 4,
      partnradu25 == 5 | partnradu25 == 9 ~ 5,
      partnradu25 < 0 ~ partnradu25,
      TRUE ~ -3
    )
  )

# --- Age 32 (Wave 9) ---
map_w9 <- c(
  '-9' = -9,
  '-8' = -8
)

merged_df <- merged_df %>%
  mutate(
    partnradu32 = map_vector(W9DMARSTAT, map_w9),
    partnr32 = case_when(
      partnradu32 == 1 | partnradu32 == 7 ~ 1,
      partnradu32 == 2 | partnradu32 == 6 ~ 2,
      partnradu32 == 4 ~ 3,
      partnradu32 == 3 ~ 4,
      partnradu32 == 5 | partnradu32 == 8 ~ 5,
      partnradu32 < 0 ~ partnradu32,
      TRUE ~ -3
    )
  )

# 4. Final Labels and Factors
partnr_labels <- c(
  "1" = "Single",
  "2" = "Married/CP",
  "3" = "Separated",
  "4" = "Divorced",
  "5" = "Widowed",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Script error",
  "-1" = "Not applicable"
)

partnradu25_labels <- c(
  "1" = "Single and never married or in a CP",
  "2" = "Married",
  "3" = "Separated but still legally married",
  "4" = "Divorced",
  "5" = "Widowed",
  "6" = "A Civil Partner",
  "7" = "Separated but still legally in a CP",
  "8" = "A former Civil Partner",
  "9" = "A surviving Civil Partner",
  "-9" = "Refusal",
  "-8" = "Insufficient information",
  "-1" = "Not applicable",
  "-3" = "Not asked"
)

partnradu32_labels <- c(
  "1" = "Single that is never married or never in a Civil Partnership",
  "2" = "Married",
  "3" = "Divorced",
  "4" = "Legally separated",
  "5" = "Widowed",
  "6" = "A Civil Partner in a legally recognised Civil Partnership",
  "7" = "A former Civil Partner (where Civil Partnership legally dissolved)",
  "8" = "A surviving Civil Partner (where Civil Partner has died)",
  "-9" = "Refusal",
  "-8" = "Insufficient information",
  "-3" = "Not asked"
)

final_df <- merged_df %>%
  select(NSID, partnr19, partnr25, partnradu25, partnr32, partnradu32) %>%
  mutate(
    partnr19 = factor(partnr19, levels = as.numeric(names(partnr_labels)), labels = partnr_labels),
    partnr25 = factor(partnr25, levels = as.numeric(names(partnr_labels)), labels = partnr_labels),
    partnr32 = factor(partnr32, levels = as.numeric(names(partnr_labels)), labels = partnr_labels),
    partnradu25 = factor(partnradu25, levels = as.numeric(names(partnradu25_labels)), labels = partnradu25_labels),
    partnradu32 = factor(partnradu32, levels = as.numeric(names(partnradu32_labels)), labels = partnradu32_labels)
  )

write_csv(final_df, 'data/output/cleaned_data.csv')
