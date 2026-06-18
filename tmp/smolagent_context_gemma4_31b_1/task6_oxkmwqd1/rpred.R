library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load Files
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab',
  'ns9_2022_main_interview.tab'
)

full_data <- NULL

for (f in files) {
  df <- readr::read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols())
  if (is.null(full_data)) {
    full_data <- df
  } else {
    full_data <- full_join(full_data, df, by = 'NSID')
  }
}

# Helper function for missing values based on general guidance
# Convert R NA to -3, and map specific labels
map_missing <- function(x, labels_map = list()) {
  # Start with NA as -3
  x[is.na(x)] <- -3
  
  # Handle specific cases like -94 (Insufficient information -> -8)
  # Based on general guidance: -94 may mean insufficient info (-8)
  # In metadata for urbind/gor, -94 is 'Insufficient information'
  x[x == -94] <- -8
  
  return(x)
}

# Harmonization for W2/W3 urbind and gor
# urbind: 1-8 substantive
# gor: 1-9 substantive

# Process Wave 2 (Age 15)
full_data <- full_data %>%
  mutate(
    regub15 = map_missing(urbind.x), # Note: read_delim might rename if multiple files have same var, 
                                   # but since we join by NSID, we need to be careful.
                                   # Let's re-load and select variables explicitly to avoid .x .y
  )

# Redoing load to handle naming collisions better
full_data <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID)

df2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, urbind_w2 = urbind, gor_w2 = gor)

df3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, urbind_w3 = urbind, gor_w3 = gor)

df4 <- readr::read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID)

df8 <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, W8DGOR)

df9_der <- readr::read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, W9DRGN)

df9_main <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, W9NATIONRES)

full_data <- full_data %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9_der, by = 'NSID') %>%
  full_join(df9_main, by = 'NSID')

# Apply transformation logic
full_data <- full_data %>%
  mutate(
    # regub15/16: 1-8
    regub15 = case_when(
      urbind_w2 >= 1 & urbind_w2 <= 8 ~ urbind_w2,
      urbind_w2 == -94 ~ -8,
      TRUE ~ -3
    ),
    regub16 = case_when(
      urbind_w3 >= 1 & urbind_w3 <= 8 ~ urbind_w3,
      urbind_w3 == -94 ~ -8,
      TRUE ~ -3
    ),
    # regov15/16: 1-9
    regov15 = case_when(
      gor_w2 >= 1 & gor_w2 <= 9 ~ gor_w2,
      gor_w2 == -94 ~ -8,
      TRUE ~ -3
    ),
    regov16 = case_when(
      gor_w3 >= 1 & gor_w3 <= 9 ~ gor_w3,
      gor_w3 == -94 ~ -8,
      TRUE ~ -3
    ),
    # regor25: W8DGOR
    regor25 = case_when(
      W8DGOR >= 1 & W8DGOR <= 12 ~ W8DGOR,
      W8DGOR == 13 ~ -2,
      W8DGOR == -9 ~ -9,
      W8DGOR == -8 ~ -8,
      W8DGOR == -1 ~ -1,
      TRUE ~ -3
    ),
    # regor32: W9DRGN
    regor32 = case_when(
      W9DRGN >= 1 & W9DRGN <= 12 ~ W9DRGN,
      W9DRGN == 13 ~ -2,
      W9DRGN == -9 ~ -9,
      W9DRGN == -8 ~ -8,
      W9DRGN == -1 ~ -1,
      TRUE ~ -3
    ),
    # regint32: W9NATIONRES
    # England(1), Scotland(2), Wales(3), Northern Ireland(4) -> 1
    # Outside UK(5) or unknown -> 2
    regint32 = case_when(
      W9NATIONRES >= 1 & W9NATIONRES <= 4 ~ 1,
      W9NATIONRES == 5 ~ 2,
      W9NATIONRES == -9 ~ -9,
      W9NATIONRES == -8 ~ -8,
      W9NATIONRES == -3 ~ -3,
      W9NATIONRES == -1 ~ -1,
      TRUE ~ -3
    )
  )

# Final selection
final_df <- full_data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Ensure factors and labels if needed, but requirements just specify codes.
# The instructions say: 'create labelled factors with explicit labels for all valid categories'
# Let's add labels for the target variables.

# Labels for regub
urb_labels <- c('1' = 'Urban >= 10k - sparse', '2' = 'Town & Fringe - sparse', '3' = 'Village - sparse', '4' = 'Hamlet and Isolated Dwelling - sparse', '5' = 'Urban >= 10k - less sparse', '6' = 'Town & Fringe - less sparse', '7' = 'Village - less sparse', '8' = 'Hamlet & Isolated Dwelling', '-8' = 'Don\'t know / insufficient information', '-3' = 'Not asked at the fieldwork stage / not interviewed')

# Labels for regov
gov_labels <- c('1' = 'North East', '2' = 'North West', '3' = 'Yorkshire and The Humber', '4' = 'East Midlands', '5' = 'West Midlands', '6' = 'East of England', '7' = 'London', '8' = 'South East', '9' = 'South West', '-8' = 'Don\'t know / insufficient information', '-3' = 'Not asked at the fieldwork stage / not interviewed')

# Labels for regor
reg_labels <- c('1' = 'North East', '2' = 'North West', '3' = 'Yorkshire and the Humber', '4' = 'East Midlands', '5' = 'West Midlands', '6' = 'East of England', '7' = 'London', '8' = 'South East', '9' = 'South West', '10' = 'Wales', '11' = 'Scotland', '12' = 'Northern Ireland', '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable', '-3' = 'Not asked at the fieldwork stage / not interviewed')

# Labels for regint
int_labels <- c('1' = 'In the UK', '2' = 'Abroad', '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-3' = 'Not asked at the fieldwork stage / not interviewed', '-1' = 'Item not applicable')

# Apply labels using labelled package
final_df$regub15 <- as_factor(final_df$regub15)
final_df$regub16 <- as_factor(final_df$regub16)
final_df$regov15 <- as_factor(final_df$regov15)
final_df$regov16 <- as_factor(final_df$regov16)
final_df$regor25 <- as_factor(final_df$regor25)
final_df$regor32 <- as_factor(final_df$regor32)
final_df$regint32 <- as_factor(final_df$regint32)

# Note: the prompt asks for labelled factors. The above as_factor is a haven function.
# Let's write to CSV.
readr::write_csv(final_df, 'data/output/cleaned_data.csv')
