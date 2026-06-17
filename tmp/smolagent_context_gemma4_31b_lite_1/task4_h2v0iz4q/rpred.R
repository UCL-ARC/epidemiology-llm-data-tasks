library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

load_tab <- function(fname) {
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'double'))
}

# We need to ensure NSID is read as character for joining
load_tab_fixed <- function(fname) {
  readr::read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'double'))
}

data1 <- load_tab_fixed('wave_one_lsype_young_person_2020.tab')
data4 <- load_tab_fixed('wave_four_lsype_young_person_2020.tab')
data6 <- load_tab_fixed('wave_six_lsype_young_person_2020.tab')
data7 <- load_tab_fixed('wave_seven_lsype_young_person_2020.tab')
data8 <- load_tab_fixed('ns8_2015_self_completion.tab')
data9 <- load_tab_fixed('ns9_2022_main_interview.tab')

# Merge datasets
full_df <- data1 %>%
  full_join(data4, by = 'NSID') %>%
  full_join(data6, by = 'NSID') %>%
  full_join(data7, by = 'NSID') %>%
  full_join(data8, by = 'NSID') %>%
  full_join(data9, by = 'NSID')

# Helper function for mapping missing values based on labels
# Standard Missing-Value Codes:
# -9 = Refusal
# -8 = Don't know
# -7 = Prefer not to say
# -3 = Not asked
# -2 = Schedule not applicable / script error / info lost
# -1 = Item not applicable

clean_sori <- function(var_vec, wave_name) {
  res <- var_vec
  
  # Map by meaning based on metadata
  if (wave_name == 'W6') {
    # W6SexualityYP: -97: Refused self completion (-2), -92: Refused (-9), -91: Not applicable (-1), -1: Don't know (-8)
    res <- case_when(
      var_vec == -97 ~ -2,
      var_vec == -92 ~ -9,
      var_vec == -91 ~ -1,
      var_vec == -1  ~ -8,
      var_vec >= 1    ~ var_vec,
      TRUE           ~ -3
    )
  } else if (wave_name == 'W7') {
    # W7SexualityYP: -100: Respondent declined sexual experience questions (-2), -97: Refused self completion (-2), -92: Refused (-9), -91: Not applicable (-1), -1: Don't know (-8)
    res <- case_when(
      var_vec == -100 ~ -2,
      var_vec == -97  ~ -2,
      var_vec == -92  ~ -9,
      var_vec == -91  ~ -1,
      var_vec == -1   ~ -8,
      var_vec >= 1    ~ var_vec,
      TRUE           ~ -3
    )
  } else if (wave_name == 'W8') {
    # W8SEXUALITY: -9: Refused (-9), -8: Don't know (-8), -1: Not applicable (-1)
    res <- case_when(
      var_vec == -9 ~ -9,
      var_vec == -8 ~ -8,
      var_vec == -1 ~ -1,
      var_vec >= 1  ~ var_vec,
      TRUE         ~ -3
    )
  } else if (wave_name == 'W9') {
    # W9SORI: -9: Refused (-9), -8: Don't know (-8), -3: Not asked (-3), -1: Not applicable (-1), 5: Prefer not to say (-7)
    res <- case_when(
      var_vec == -9 ~ -9,
      var_vec == -8 ~ -8,
      var_vec == -3 ~ -3,
      var_vec == -1 ~ -1,
      var_vec == 5  ~ -7,
      var_vec >= 1 & var_vec <= 4 ~ var_vec,
      TRUE         ~ -3
    )
  }
  
  # Final NA to -3
  res[is.na(res)] <- -3
  return(res)
}

# Apply cleaning
full_df <- full_df %>%
  mutate(
    sori19 = clean_sori(W6SexualityYP, 'W6'),
    sori20 = clean_sori(W7SexualityYP, 'W7'),
    sori25 = clean_sori(W8SEXUALITY, 'W8'),
    sori32 = clean_sori(W9SORI, 'W9')
  )

# Define factor labels for the output
# Categories: 1: Heterosexual / Straight, 2: Gay / Lesbian, 3: Bisexual, 4: Other
# Missing: -9: Refused, -8: Don't know, -7: Prefer not to say, -3: Not asked, -2: Schedule not applicable, -1: Item not applicable

sori_labels <- c(
  "1" = "Heterosexual / Straight",
  "2" = "Gay / Lesbian",
  "3" = "Bisexual",
  "4" = "Other",
  "-9" = "Refused",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Convert to factors
factor_sori <- function(x) {
  # Ensure we only keep labels that exist in the data or are required by the standard
  # We use the order from the source (1,2,3,4 then missing codes)
  levels_order <- c("1", "2", "3", "4", "-9", "-8", "-7", "-3", "-2", "-1")
  f <- factor(as.character(x), levels = levels_order, labels = sori_labels[levels_order])
  return(f)
}

full_df <- full_df %>%
  mutate(
    sori19 = factor_sori(sori19),
    sori20 = factor_sori(sori20),
    sori25 = factor_sori(sori25),
    sori32 = factor_sori(sori32)
  )

# Keep only ID and derived variables
final_output <- full_df %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write CSV
readr::write_csv(final_output, 'data/output/cleaned_data.csv')
