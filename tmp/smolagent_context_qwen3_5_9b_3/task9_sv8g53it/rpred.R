library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Load the three wave files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Full join all waves on NSID - keep all 15760 cases
combined <- full_join(wave1, wave2, by = 'NSID')
combined <- full_join(combined, wave4, by = 'NSID')

# Rename qualification columns for clarity
combined <- combined %>%
  rename(
    mum_w1 = W1hiqualmum,
    dad_w1 = W1hiqualdad,
    mum_w2 = W2hiqualmum,
    dad_w2 = W2hiqualdad,
    mum_w4 = w4hiqualmum,
    dad_w4 = w4hiqualdad
  )

# Harmonize missing values within the combined dataframe
combined <- combined %>%
  mutate(
    mum_w1 = case_when(
      mum_w1 == -999 ~ -2, mum_w1 == -99 ~ -8, mum_w1 == -98 ~ -3,
      mum_w1 == -94 ~ -8, mum_w1 == -92 ~ -9, mum_w1 == -91 ~ -1,
      mum_w1 == -1 ~ -8, is.na(mum_w1) ~ -3, TRUE ~ mum_w1
    ),
    dad_w1 = case_when(
      dad_w1 == -999 ~ -2, dad_w1 == -99 ~ -8, dad_w1 == -98 ~ -3,
      dad_w1 == -94 ~ -8, dad_w1 == -92 ~ -9, dad_w1 == -91 ~ -1,
      dad_w1 == -1 ~ -8, is.na(dad_w1) ~ -3, TRUE ~ dad_w1
    ),
    mum_w2 = case_when(
      mum_w2 == -999 ~ -2, mum_w2 == -99 ~ -8, mum_w2 == -98 ~ -3,
      mum_w2 == -94 ~ -8, mum_w2 == -92 ~ -9, mum_w2 == -91 ~ -1,
      mum_w2 == -1 ~ -8, is.na(mum_w2) ~ -3, TRUE ~ mum_w2
    ),
    dad_w2 = case_when(
      dad_w2 == -999 ~ -2, dad_w2 == -99 ~ -8, dad_w2 == -98 ~ -3,
      dad_w2 == -94 ~ -8, dad_w2 == -92 ~ -9, dad_w2 == -91 ~ -1,
      dad_w2 == -1 ~ -8, is.na(dad_w2) ~ -3, TRUE ~ dad_w2
    ),
    mum_w4 = case_when(
      mum_w4 == -99 ~ -8, mum_w4 == -98 ~ -3, mum_w4 == -94 ~ -8,
      is.na(mum_w4) ~ -3, TRUE ~ mum_w4
    ),
    dad_w4 = case_when(
      dad_w4 == -99 ~ -8, dad_w4 == -98 ~ -3, dad_w4 == -94 ~ -8,
      is.na(dad_w4) ~ -3, TRUE ~ dad_w4
    )
  )

# Derive detailed consolidated variables
derive_detailed <- function(w1, w2, w4) {
  result <- case_when(
    !is.na(w1) & w1 >= 1 & w1 <= 20 ~ w1,
    !is.na(w2) & w2 >= 1 & w2 <= 20 ~ w2,
    !is.na(w4) & w4 >= 1 & w4 <= 20 ~ w4,
    !is.na(w1) & w1 < 0 & w1 >= -100 ~ w1,
    !is.na(w2) & w2 < 0 & w2 >= -100 ~ w2,
    !is.na(w4) & w4 < 0 & w4 >= -100 ~ w4,
    TRUE ~ -3
  )
  return(result)
}

detailed_mom <- derive_detailed(combined$mum_w1, combined$mum_w2, combined$mum_w4)
detailed_dad <- derive_detailed(combined$dad_w1, combined$dad_w2, combined$dad_w4)

# Create collapsed NVQ from detailed
code_to_nvq <- function(detailed) {
  case_when(
    detailed %in% c(1, 2, 3, 4) ~ 0,
    detailed %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,
    detailed == 18 ~ 2,
    detailed == 19 ~ 3,
    detailed == 20 ~ 4,
    TRUE ~ -3
  )
}

nvq_mom <- code_to_nvq(detailed_mom)
nvq_dad <- code_to_nvq(detailed_dad)

# Create final output with only required variables
output <- combined %>%
  mutate(
    educdtlma = detailed_mom,
    educdtlpa = detailed_dad,
    educma = nvq_mom,
    educpa = nvq_dad
  ) %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(output), '\n')
cat('Number of columns:', ncol(output), '\n')
cat('Variables:', names(output), '\n')