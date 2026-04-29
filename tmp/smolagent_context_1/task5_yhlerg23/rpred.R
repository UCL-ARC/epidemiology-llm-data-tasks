library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define files based on metadata
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load files
load_tab <- function(fname) {
  read_delim(paste0('data/input/', fname), delim = '\t', col_types = readr::cols(.default = 'c'))
}

# We load them into a list first to merge
data_list <- map(files, load_tab)
names(data_list) <- files

# Merge datasets by NSID
full_df <- data_list[[1]] %>% 
  full_join(data_list[[2]], by = 'NSID') %>% 
  full_join(data_list[[3]], by = 'NSID') %>% 
  full_join(data_list[[4]], by = 'NSID') %>% 
  full_join(data_list[[5]], by = 'NSID')

# Convert numeric variables to numeric type as read_delim loaded them as characters
full_df <- full_df %>%
  mutate(across(c(W6MarStatYP, W8DMARSTAT, W9DMARSTAT), as.numeric))

# Mapping function for missing values based on rules
# -9 = Refusal, -8 = Don't know/insufficient, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule NA, -1 = Item NA

# Process Wave 6 (Age 19)
# W6MarStatYP: -997: Script error (-2), -97: Declined (-7), -92: Refused (-9), -91: Not applicable (-1), -1: Don't know (-8)
full_df <- full_df %>%
  mutate(
    partnr19 = case_when(
      W6MarStatYP == 1 ~ 1,
      W6MarStatYP == 2 ~ 2,
      W6MarStatYP == 3 ~ 3,
      W6MarStatYP == 4 ~ 4,
      W6MarStatYP == 5 ~ 5,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -97 ~ -7,
      W6MarStatYP == -1 ~ -8,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -997 ~ -2,
      TRUE ~ -3
    )
  )

# Process Wave 8 (Age 25 approx, derived file)
# W8DMARSTAT: -9: Refused (-9), -8: Insufficient (-8), -1: Not applicable (-1)
# Categories: 1: Single, 2: Married, 3: Separated, 4: Divorced, 5: Widowed, 6: CP, 7: Sep CP, 8: Former CP, 9: Surv CP
# Harmonised (partnr25): 1: Single/CP, 2: Married/CP, 3: Separated, 4: Divorced/Former CP, 5: Widowed/Surviving CP
full_df <- full_df %>%
  mutate(
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 6 ~ 1, # CP is like married/single depending on context, but usually grouped as partnership
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 6 ~ 2, # Mapping CP to Married for harmonised
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 7 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 8 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 9 ~ 5,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    ),
    # Correcting CP logic: CP (6) should be harmonised as Married (2) or Single (1)? 
    # Standard harmonisation: 1=Single, 2=Married/CP, 3=Separated, 4=Divorced/Dissolved, 5=Widowed
    partnr25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 6 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 7 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 8 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 9 ~ 5,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    ),
    partnradu25 = case_when(
      W8DMARSTAT == 1 ~ 1,
      W8DMARSTAT == 2 ~ 2,
      W8DMARSTAT == 3 ~ 3,
      W8DMARSTAT == 4 ~ 4,
      W8DMARSTAT == 5 ~ 5,
      W8DMARSTAT == 6 ~ 6,
      W8DMARSTAT == 7 ~ 7,
      W8DMARSTAT == 8 ~ 8,
      W8DMARSTAT == 9 ~ 9,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Process Wave 9 (Age 32)
# W9DMARSTAT: -9: Refused, -8: Insufficient
# Categories: 1: Single, 2: Married, 3: Divorced, 4: Legally separated, 5: Widowed, 6: CP, 7: Former CP, 8: Surv CP
full_df <- full_df %>%
  mutate(
    partnr32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 6 ~ 2,
      W9DMARSTAT == 4 ~ 3,
      W9DMARSTAT == 3 ~ 4,
      W9DMARSTAT == 7 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 8 ~ 5,
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      TRUE ~ -3
    ),
    partnradu32 = case_when(
      W9DMARSTAT == 1 ~ 1,
      W9DMARSTAT == 2 ~ 2,
      W9DMARSTAT == 3 ~ 3,
      W9DMARSTAT == 4 ~ 4,
      W9DMARSTAT == 5 ~ 5,
      W9DMARSTAT == 6 ~ 6,
      W9DMARSTAT == 7 ~ 7,
      W9DMARSTAT == 8 ~ 8,
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      TRUE ~ -3
    )
  )

# Final Selection
final_df <- full_df %>%
  select(NSID, partnr19, partnr25, partnradu25, partnr32, partnradu32)

# Define labels
val_labels <- c(
  "1" = "Single",
  "2" = "Married/CP",
  "3" = "Separated",
  "4" = "Divorced/Former CP",
  "5" = "Widowed/Surviving CP",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule NA",
  "-1" = "Not applicable"
)

# Applying factors for the harmonised variables
final_df <- final_df %>%
  mutate(across(starts_with("partnr"), ~ factor(.x, levels = as.numeric(names(val_labels)), labels = val_labels)))

write_csv(final_df, 'data/output/cleaned_data.csv')
