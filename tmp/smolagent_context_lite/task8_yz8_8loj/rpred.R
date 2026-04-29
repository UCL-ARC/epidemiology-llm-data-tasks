library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(tidyr)

# File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_main_interview.tab',
  'ns9_2022_derived_variables.tab'
)

data_list <- map(files, ~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols()))
names(data_list) <- files

# Merge datasets using full_join by NSID
full_df <- data_list %>% reduce(full_join, by = 'NSID')

# Function to map NVQ levels to 1-5, others to 0 (None) or missing
map_nvq <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 4,
    x == 5 ~ 5,
    x == 0 | x == 96 ~ 0,
    x == 95 ~ 0,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ -3
  )
}

full_df <- full_df %>%
  mutate(
    # Age 25 (Wave 8) - Using W8DHANVQH
    educ25_raw = W8DHANVQH,
    educ25 = case_when(
      educ25_raw == 1 ~ 1,
      educ25_raw == 2 ~ 2,
      educ25_raw == 3 ~ 3,
      educ25_raw == 4 ~ 4,
      educ25_raw == 5 ~ 5,
      educ25_raw == 96 | educ25_raw == 95 ~ 0,
      educ25_raw == -9 ~ -9,
      educ25_raw == -8 ~ -8,
      educ25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    
    # Age 32 (Wave 9)
    educ32_acad = map_nvq(W9DANVQH),
    educ32_voc = map_nvq(W9DVNVQH),
    educ32 = pmax(educ32_acad, educ32_voc, na.rm = TRUE),
    
    # Detailed academic Age 32
    educadtl32 = case_when(
      W9ACQU0A == 1 ~ "Doctorate",
      W9ACQU0B == 1 ~ "Masters",
      W9ACQU0C == 1 ~ "Undergraduate",
      W9ACQU0D == 1 ~ "Post-grad Dip",
      W9ACQU0E == 1 ~ "Higher Ed Dip",
      W9ACQU0F == 1 ~ "Teaching Qual",
      W9ACQU0G == 1 ~ "A-Levels",
      W9ACQU0H == 1 ~ "GCSE A-C",
      W9ACQU0I == 1 ~ "GCSE D-G",
      W9ACQU0S == 1 ~ "None",
      TRUE ~ NA_character_
    ),
    
    # Detailed vocational Age 32
    educvdtl32 = case_when(
      W9VCQU0A == 1 ~ "Prof Degree",
      W9VCQU0B == 1 ~ "Medical below deg",
      W9VCQU0C == 1 ~ "Level 4/5",
      W9VCQU0D == 1 ~ "Level 3",
      W9VCQU0E == 1 ~ "Level 2",
      W9VCQU0F == 1 ~ "Level 1",
      W9VCQUAG == 1 ~ "None",
      TRUE ~ NA_character_
    )
  )

# Final cleanup using tidyr::replace_na
full_df <- full_df %>%
  mutate(
    educadtl32 = replace_na(educadtl32, "Not asked/NA"),
    educvdtl32 = replace_na(educvdtl32, "Not asked/NA")
  )

final_output <- full_df %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

write_csv(final_output, 'data/output/cleaned_data.csv')