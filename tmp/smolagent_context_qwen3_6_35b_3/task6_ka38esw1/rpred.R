library(dplyr)
library(readr)
library(tidyr)
library(labelled)

# Load all files explicitly by name from metadata
w1 <- read_tsv('data/input/wave_one_lsype_young_person_2020.tab', show_col_types = FALSE)
w4 <- read_tsv('data/input/wave_four_lsype_young_person_2020.tab', show_col_types = FALSE)
w2 <- read_tsv('data/input/wave_two_lsype_family_background_2020.tab', show_col_types = FALSE)
w3 <- read_tsv('data/input/wave_three_lsype_family_background_2020.tab', show_col_types = FALSE)
w8_derived <- read_tsv('data/input/ns8_2015_derived.tab', show_col_types = FALSE)
w9_derived <- read_tsv('data/input/ns9_2022_derived_variables.tab', show_col_types = FALSE)
w9_main <- read_tsv('data/input/ns9_2022_main_interview.tab', show_col_types = FALSE)

# Merge all datasets by NSID
df <- w1 %>%
  full_join(w4, by = 'NSID') %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w8_derived, by = 'NSID') %>%
  full_join(w9_derived, by = 'NSID') %>%
  full_join(w9_main, by = 'NSID')

cat('Merged dataset dimensions:', dim(df), '\n')

# Helper function to map missing values and recode
recode_geo <- function(x, source_13_to_minus2 = FALSE) {
  result <- as.numeric(x)
  
  # Map source value 13 to -2 if required (for W8DGOR and W9DRGN)
  if (source_13_to_minus2) {
    result[result == 13] <- -2
  }
  
  # Map standard missing value codes by label meaning
  # -94 (Insufficient information) -> -8
  result[result == -94] <- -8
  # -99 -> -3 (Not asked at fieldwork stage)
  result[result == -99] <- -3
  # -92 -> -9 (Refused)
  result[result == -92] <- -9
  # -91 -> -1 (Not applicable)
  result[result == -91] <- -1
  # -999, -998, -997 -> -2 (Schedule not applicable / script error / info lost)
  result[result %in% c(-999, -998, -997)] <- -2
  
  # Convert remaining NAs to -3 (Not asked at fieldwork stage)
  result[is.na(result)] <- -3
  
  return(result)
}

# Build final output with only NSID and derived variables
out <- df %>%
  mutate(
    # regub15 from W2 urbind (Age 15) - Urban/Rural Indicator
    regub15 = recode_geo(urbind.x),
    # regov15 from W2 gor (Age 15) - Government Office Region
    regov15 = recode_geo(gor.x),
    # regub16 from W3 urbind (Age 16) - Urban/Rural Indicator
    regub16 = recode_geo(urbind.y),
    # regov16 from W3 gor (Age 16) - Government Office Region
    regov16 = recode_geo(gor.y),
    # regor25 from W8DGOR (Age 25) - UK regions
    # Map source value 13 to -2 as specified
    regor25 = recode_geo(W8DGOR, source_13_to_minus2 = TRUE),
    # regor32 from W9DRGN (Age 32) - UK regions
    # Map source value 13 to -2 as specified
    regor32 = recode_geo(W9DRGN, source_13_to_minus2 = TRUE),
    # regint32 from W9NATIONRES (Age 32)
    # 1 = In the UK, 2 = Abroad
    regint32 = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,  # England, Scotland, Wales, Northern Ireland -> In UK
      W9NATIONRES == 5 ~ 2,                 # Outside of UK or unknown -> Abroad
      W9NATIONRES == -9 ~ -9,               # Refused
      W9NATIONRES == -8 ~ -8,               # Don't know
      W9NATIONRES == -3 ~ -3,               # Not asked at fieldwork stage
      W9NATIONRES == -1 ~ -1,               # Not applicable
      TRUE ~ -3                             # Default for NAs or other -> Not asked
    )
  ) %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

# Write output
dir.create('data/output', showWarnings = FALSE)
write_csv(out, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Output dimensions:', dim(out), '\n')
cat('Output variables:', names(out), '\n')

# Summary of each variable
cat('\n--- Summary statistics ---\n')
for (v in names(out)) {
  cat(sprintf('\n%s: \n', v))
  cat(table(out[[v]], useNA = 'always'), '\n')
}
