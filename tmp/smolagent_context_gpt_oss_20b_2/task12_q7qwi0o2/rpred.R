library(readr)
library(dplyr)
library(tidyr)
library(purrr)

# Define file paths
files <- list(
  wave_one = 'data/input/wave_one_lsype_young_person_2020.tab',
  wave_five = 'data/input/wave_five_lsype_young_person_2020.tab',
  wave_six = 'data/input/wave_six_lsype_young_person_2020.tab',
  wave_seven = 'data/input/wave_seven_lsype_young_person_2020.tab',
  ns8 = 'data/input/ns8_2015_derived.tab',
  ns9 = 'data/input/ns9_2022_main_interview.tab',
  wave_four = 'data/input/wave_four_lsype_young_person_2020.tab'
)

# Function to read a tab‑delimited file
read_tab <- function(path){
  if(file.exists(path)){
    read_delim(path, delim='\t', col_types = cols(), progress = FALSE)
  } else {
    tibble()
  }
}

# Read all files
all_data <- lapply(files, read_tab)

# Merge all datasets on NSID
merged <- all_data[[1]]
for(i in 2:length(all_data)){
  df <- all_data[[i]]
  if(!'NSID' %in% names(df)) next
  merged <- full_join(merged, df, by='NSID')
}

# Helper: collapse fractional NS‑SEC codes and apply missing rules
collapse_nssec <- function(raw, dact=NULL, special=FALSE){
  raw_num <- as.numeric(raw)
  raw_mapped <- case_when(
    is.na(raw_num) ~ -3L,
    raw_num == -9 ~ -9L,
    raw_num == -8 ~ -8L,
    raw_num == -7 ~ -7L,
    raw_num == -3 ~ -3L,
    raw_num == -2 ~ -2L,
    raw_num == -1 ~ -1L,
    raw_num == -91 ~ -1L,
    raw_num == -99 ~ -3L,
    TRUE ~ raw_num
  )
  raw_int <- ifelse(raw_mapped > 0, floor(raw_mapped), raw_mapped)
  if(special && !is.null(dact)){
    dact_num <- as.numeric(dact)
    if(any(dact_num == 5L, na.rm = TRUE)){
      return(15L)
    }
  }
  raw_int
}

# Derive NS‑SEC variables for each age
merged <- merged %>%
  mutate(
    nssec17 = collapse_nssec(W4nsseccatYP),
    nssec18 = collapse_nssec(W5nsseccatYP),
    nssec19 = collapse_nssec(w6nsseccatYP),
    nssec20 = collapse_nssec(W7NSSECCat),
    nssec25 = collapse_nssec(W8DNSSEC17, dact = W8DACTIVITYC, special = TRUE),
    nssec32 = ifelse('W9NSSEC' %in% names(.), collapse_nssec(W9NSSEC), -3L)
  )

# Keep only the required final variables
final_df <- merged %>% select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write the cleaned data to CSV
write_csv(final_df, 'data/output/cleaned_data.csv')