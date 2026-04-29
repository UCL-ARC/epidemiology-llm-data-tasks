library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path_prefix <- "data/input/"

# Wave 1
w1 <- read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c")) %>% select(NSID)

# Wave 2
w2_raw <- read_delim(paste0(path_prefix, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "c", urbind = "d", gor = "d"))
w2 <- w2_raw %>% 
  mutate(
    regub15 = case_when(
      urbind == -94 ~ -2,
      urbind >= 1 & urbind <= 8 ~ urbind,
      urbind >= -999 & urbind <= -1 ~ urbind,
      TRUE ~ -3
    ), 
    regov15 = case_when(
      gor == -94 ~ -2,
      gor >= 1 & gor <= 9 ~ gor,
      gor >= -999 & gor <= -1 ~ gor,
      TRUE ~ -3
    )
  ) %>% 
  select(NSID, regub15, regov15)

# Wave 3
w3_raw <- read_delim(paste0(path_prefix, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = readr::cols(NSID = "c", urbind = "d", gor = "d"))
w3 <- w3_raw %>% 
  mutate(
    regub16 = case_when(
      urbind == -94 ~ -2,
      urbind >= 1 & urbind <= 8 ~ urbind,
      urbind >= -999 & urbind <= -1 ~ urbind,
      TRUE ~ -3
    ), 
    regov16 = case_when(
      gor == -94 ~ -2,
      gor >= 1 & gor <= 9 ~ gor,
      gor >= -999 & gor <= -1 ~ gor,
      TRUE ~ -3
    )
  ) %>% 
  select(NSID, regub16, regov16)

# Wave 4
w4 <- read_delim(paste0(path_prefix, "wave_four_lsype_young_person_2020.tab"), delim = "\t", col_types = readr::cols(.default = "c")) %>% select(NSID)

# Wave 8
w8_raw <- read_delim(paste0(path_prefix, "ns8_2015_derived.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W8DGOR = "d"))
w8 <- w8_raw %>% 
  mutate(
    regor25 = case_when(
      W8DGOR >= 1 & W8DGOR <= 13 ~ W8DGOR,
      W8DGOR == -9 ~ -9, 
      W8DGOR == -8 ~ -8, 
      W8DGOR == -1 ~ -1, 
      TRUE ~ -3
    )
  ) %>% 
  select(NSID, regor25)

# Wave 9 Derived
w9_der_raw <- read_delim(paste0(path_prefix, "ns9_2022_derived_variables.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W9DRGN = "d"))
w9_der <- w9_der_raw %>% 
  mutate(
    regor32 = case_when(
      W9DRGN >= 1 & W9DRGN <= 13 ~ W9DRGN,
      W9DRGN == -9 ~ -9, 
      W9DRGN == -8 ~ -8, 
      W9DRGN == -1 ~ -1, 
      TRUE ~ -3
    )
  ) %>% 
  select(NSID, regor32)

# Wave 9 Main
w9_main_raw <- read_delim(paste0(path_prefix, "ns9_2022_main_interview.tab"), delim = "\t", col_types = readr::cols(NSID = "c", W9NATIONRES = "d"))
w9_main <- w9_main_raw %>% 
  mutate(
    regint32 = case_when(
      W9NATIONRES >= 1 & W9NATIONRES <= 4 ~ 1,
      W9NATIONRES == 5 ~ 2,
      W9NATIONRES == -9 ~ -9, 
      W9NATIONRES == -8 ~ -8, 
      W9NATIONRES == -3 ~ -3, 
      W9NATIONRES == -1 ~ -1, 
      TRUE ~ -3
    )
  ) %>% 
  select(NSID, regint32)

# 2. Merge datasets
final_df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9_der, by = "NSID") %>%
  full_join(w9_main, by = "NSID")

# Final cleanup of NAs to -3
vars_to_fix <- c("regub15", "regov15", "regub16", "regov16", "regor25", "regor32", "regint32")
for(v in vars_to_fix) {
  if(v %in% names(final_df)) {
    final_df[[v]][is.na(final_df[[v]])] <- -3
  }
}

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")