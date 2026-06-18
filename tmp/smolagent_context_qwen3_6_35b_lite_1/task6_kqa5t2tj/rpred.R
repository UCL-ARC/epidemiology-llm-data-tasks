library(dplyr)
library(readr)
library(labelled)
library(tidyr)

# Create output directory
if(!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim='\t', show_col_types=FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim='\t', show_col_types=FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim='\t', show_col_types=FALSE)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim='\t', show_col_types=FALSE)
ns8 <- read_delim('data/input/ns8_2015_derived.tab', delim='\t', show_col_types=FALSE)
ns9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim='\t', show_col_types=FALSE)
ns9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim='\t', show_col_types=FALSE)

# Merge all files by NSID
full <- full_join(wave1, wave4, by='NSID')
full <- full_join(full, wave2, by='NSID')
full <- full_join(full, wave3, by='NSID')
full <- full_join(full, ns8, by='NSID')
full <- full_join(full, ns9_derived, by='NSID')
full <- full_join(full, ns9_main, by='NSID')

# Helper to recode missing values using case_when
recodemm <- function(x, default_na = -3) {
  x <- as.numeric(x)
  x <- case_when(
    x == -94 ~ -2,  # Insufficient information -> schedule not applicable
    TRUE ~ x
  )
  x[is.na(x)] <- default_na
  return(x)
}

# Create derived variables directly in full dataframe
full <- full %>%
  mutate(
    # regub15 - Urban/Rural at age 15 from wave2$urbind (became urbind.x after merge with wave3)
    regub15 = recodemm(urbind.x),
    
    # regov15 - Government Office Region at age 15 from wave2$gor (became gor.x after merge with wave3)
    regov15 = recodemm(gor.x),
    
    # regub16 - Urban/Rural at age 16 from wave3$urbind (became urbind.y after merge)
    regub16 = recodemm(urbind.y),
    
    # regov16 - Government Office Region at age 16 from wave3$gor (became gor.y after merge)
    regov16 = recodemm(gor.y),
    
    # regor25 - Government Office Region at age 25 from ns8$W8DGOR
    regor25 = ifelse(is.na(W8DGOR), -3, as.numeric(W8DGOR)),
    
    # regor32 - Government Office Region at age 32 from ns9_derived$W9DRGN
    regor32 = ifelse(is.na(W9DRGN), -3, as.numeric(W9DRGN)),
    
    # regint32 - UK/Abroad at age 32 from ns9_main$W9NATIONRES
    regint32 = ifelse(is.na(W9NATIONRES), -3, as.numeric(W9NATIONRES))
  )

# Select only NSID and the derived variables
result <- full %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

# Print summary
cat('Output dimensions:', dim(result), '\n')
cat('Variables:', names(result), '\n')
