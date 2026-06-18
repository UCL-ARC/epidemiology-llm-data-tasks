library(readr)
library(dplyr)
library(labelled)

# 1. Read all raw data files
wave_one  <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_two  <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_guess()))
ns9  <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = cols(.default = col_guess()))

# 2. Harmonise missing‑value codes for IMD
map_imd_missing <- function(x){
  x <- as.numeric(x)
  x <- case_when(
    x == -94.0 ~ -8,   # Insufficient information
    x == -92.0 ~ -9,   # Refusal
    x == -91.0 ~ -1,   # Item not applicable / Prefer not to say
    x == -99.0 ~ -3,   # Not asked / not interviewed
    x == -100.0 | x == -97.0 ~ -2,   # Script / schedule error
    x == -999.0 | x == -998.0 | x == -997.0 | x == -995.0 ~ -2, # Schedule not applicable
    x < 0 ~ -2,        # General negative values
    TRUE ~ x
  )
  x[is.na(x)] <- -3
  return(x)
}

# 3. Create imd15, imd16, imd32
imd15 <- if("IMDRSCORE" %in% names(wave_two)){
  map_imd_missing(wave_two$IMDRSCORE)
} else{rep(NA_real_, nrow(wave_two))}

imd16 <- if("IMDRSCORE" %in% names(wave_three)){
  map_imd_missing(wave_three$IMDRSCORE)
} else{rep(NA_real_, nrow(wave_three))}

imd32 <- if("W9DIMDD" %in% names(ns9)){
  x <- as.numeric(ns9$W9DIMDD)
  x <- case_when(
    x == -8.0 ~ -8,
    TRUE ~ x
  )
  x[is.na(x)] <- -3
  x
} else{rep(NA_real_, nrow(ns9))}

# 4. Build data frames with only NSID and new variables
imd15_df <- wave_two %>% select(NSID) %>% mutate(imd15 = imd15)
imd16_df <- wave_three %>% select(NSID) %>% mutate(imd16 = imd16)
imd32_df <- ns9 %>% select(NSID) %>% mutate(imd32 = imd32)

# 5. Merge all data frames by NSID (full join to preserve cohort frame)
merged <- imd15_df %>%
  full_join(imd16_df, by = "NSID") %>%
  full_join(imd32_df, by = "NSID") %>%
  full_join(wave_one %>% select(NSID), by = "NSID") %>%
  full_join(wave_four %>% select(NSID), by = "NSID")

# 6. Keep only the required output variables
final_df <- merged %>% select(NSID, imd15, imd16, imd32)

# 7. Write the cleaned data to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

cat("Finished writing cleaned_data.csv with", nrow(final_df), "rows\n")