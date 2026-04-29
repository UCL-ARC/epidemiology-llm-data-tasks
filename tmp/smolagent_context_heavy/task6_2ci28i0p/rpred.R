library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path_prefix <- "data/input/"

# Explicitly load all files mentioned in metadata
file_1 <- readr::read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_4 <- readr::read_delim(paste0(path_prefix, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_2_fam <- readr::read_delim(paste0(path_prefix, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_3_fam <- readr::read_delim(paste0(path_prefix, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_8_der <- readr::read_delim(paste0(path_prefix, "ns8_2015_derived.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_9_der <- readr::read_delim(paste0(path_prefix, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = readr::cols(.default = "c"))
file_9_main <- readr::read_delim(paste0(path_prefix, "ns9_2022_main_interview.tab"), delim = "\t", col_types = readr::cols(.default = "c"))

# Convert numeric columns back to numeric for processing
conv_num <- function(df, cols) {
  df %>% mutate(across(all_of(cols), as.numeric))
}

file_2_fam <- conv_num(file_2_fam, c("urbind", "gor"))
file_3_fam <- conv_num(file_3_fam, c("urbind", "gor"))
file_8_der <- conv_num(file_8_der, c("W8DGOR"))
file_9_der <- conv_num(file_9_der, c("W9DRGN"))
file_9_main <- conv_num(file_9_main, c("W9NATIONRES"))

# Rename variables before joining to avoid collisions and clarify waves
file_2_fam_ren <- file_2_fam %>% select(NSID, urbind15 = urbind, gor15 = gor)
file_3_fam_ren <- file_3_fam %>% select(NSID, urbind16 = urbind, gor16 = gor)

# Merge datasets using full_join by NSID
full_cohort <- file_1 %>%
  full_join(file_4, by = "NSID") %>%
  full_join(file_2_fam_ren, by = "NSID") %>%
  full_join(file_3_fam_ren, by = "NSID") %>%
  full_join(file_8_der, by = "NSID") %>%
  full_join(file_9_der, by = "NSID") %>%
  full_join(file_9_main, by = "NSID")

# Define standard missing mapping function
map_missing <- function(x) {
  res <- x
  # Specific requirement: map source -94 to -2
  # Map source value 13 in W8DGOR/W9DRGN to -2
  res[x == -94] <- -2
  res[x == 13] <- -2
  
  # Map NA to -3
  res[is.na(res)] <- -3
  
  # General mapping from metadata labels/patterns
  res[x %in% c(-999, -998, -997, -995)] <- -2
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -99] <- -3
  
  return(res)
}

# Derivation logic
full_cohort <- full_cohort %>%
  mutate(
    regub15 = map_missing(urbind15),
    regov15 = map_missing(gor15),
    regub16 = map_missing(urbind16),
    regov16 = map_missing(gor16),
    regor25 = map_missing(W8DGOR),
    regor32 = map_missing(W9DRGN),
    regint32 = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1, # England, Scotland, Wales, NI
      W9NATIONRES == 5 ~ 2,              # Outside UK or unknown
      W9NATIONRES == -9 ~ -9,            # Refused
      W9NATIONRES == -8 ~ -8,            # Don't know
      W9NATIONRES == -3 ~ -3,            # Not asked
      W9NATIONRES == -1 ~ -1,            # Not applicable
      TRUE ~ -3
    )
  )

# Final selection
final_data <- full_cohort %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write CSV
readr::write_csv(final_data, "data/output/cleaned_data.csv")
