library(readr)
library(dplyr)

# Function to read tab-delimited files
read_tab <- function(file){
  read_delim(paste0('data/input/',file), delim = '\t', col_types = cols(), na = c('', 'NA'))
}

# Load all specified files
wave1 <- read_tab('wave_one_lsype_young_person_2020.tab')
wave4 <- read_tab('wave_four_lsype_young_person_2020.tab')
wave2 <- read_tab('wave_two_lsype_family_background_2020.tab')
wave3 <- read_tab('wave_three_lsype_family_background_2020.tab')
ns8  <- read_tab('ns8_2015_derived.tab')
ns9_derived <- read_tab('ns9_2022_derived_variables.tab')
ns9_main <- read_tab('ns9_2022_main_interview.tab')

# Merge all datasets by NSID (full join to keep cohort frame)
merged <- wave1 %>%
  full_join(wave2, by='NSID') %>%
  full_join(wave3, by='NSID') %>%
  full_join(wave4, by='NSID') %>%
  full_join(ns8, by='NSID') %>%
  full_join(ns9_derived, by='NSID') %>%
  full_join(ns9_main, by='NSID')

# Helper functions to map missing codes
clean_urbind <- function(v){
  v[v == -94] <- -8      # insufficient information
  v[v == -999] <- -2     # schedule not applicable / error
  v[v == -1] <- -1       # not applicable
  return(v)
}
clean_gor <- function(v){
  v[v == -94] <- -8
  v[v == -999] <- -2
  v[v == -1] <- -1
  return(v)
}

# Create derived geographical variables and add to merged
merged <- merged %>%
  mutate(
    regub15 = clean_urbind(urbind.x),   # wave2
    regub16 = clean_urbind(urbind.y),   # wave3
    regor15 = clean_gor(gor.x),          # wave2
    regor16 = clean_gor(gor.y),          # wave3
    regor25 = W8DGOR,                    # ns8
    regor32 = W9DRGN,                    # ns9 derived
    regint32 = case_when(
      is.na(W9NATIONRES) ~ NA_real_,
      W9NATIONRES == 5 ~ 1L,
      W9NATIONRES %in% 1:4 ~ 0L,
      TRUE ~ as.numeric(W9NATIONRES)
    )
  )

# Keep only final variables
final_df <- merged %>%
  select(NSID, regub15, regub16, regor15, regor16, regor25, regor32, regint32)

# Write output
write_csv(final_df, 'data/output/cleaned_data.csv')
