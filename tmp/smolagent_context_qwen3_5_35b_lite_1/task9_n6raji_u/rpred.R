library(readr)
library(dplyr)
library(tidyr)
library(haven)
library(labelled)

# Load data files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all waves using full_join by NSID
df <- full_join(wave1, wave2, by = 'NSID')
df <- full_join(df, wave4, by = 'NSID')

cat('Merged data dimensions:', dim(df), '\n')

# Function to get earliest valid response
get_earliest_valid <- function(v1, v2, v4) {
  missing_codes <- c(-999, -99, -98, -94, -92, -91, -1)
  
  result <- rep(NA_real_, length(v1))
  
  for (i in seq_along(v1)) {
    if (!is.na(v1[i]) && !v1[i] %in% missing_codes) {
      result[i] <- v1[i]
    } else if (!is.na(v2[i]) && !v2[i] %in% missing_codes) {
      result[i] <- v2[i]
    } else if (!is.na(v4[i]) && !v4[i] %in% missing_codes) {
      result[i] <- v4[i]
    }
  }
  
  return(result)
}

# Get earliest valid detailed education for mother and father
df$educdtlma_raw <- get_earliest_valid(df$W1hiqualmum, df$W2hiqualmum, df$w4hiqualmum)
df$educdtlpa_raw <- get_earliest_valid(df$W1hiqualdad, df$W2hiqualdad, df$w4hiqualdad)

# Convert NA to -3 for detailed variables
df$educdtlma_raw[is.na(df$educdtlma_raw)] <- -3
df$educdtlpa_raw[is.na(df$educdtlpa_raw)] <- -3

# Map to 5-level NVQ categories using vectorized ifelse
map_to_5level <- function(x) {
  ifelse(x == 1, 5,
    ifelse(x %in% c(2, 3, 4), 4,
      ifelse(x %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13), 3,
        ifelse(x %in% c(14, 15, 16, 17), 2,
          ifelse(x %in% c(18, 19, 20), 1, NA_real_)
        )
      )
    )
  )
}

# Get earliest valid 5-level education for mother and father
df$educma_raw <- get_earliest_valid(df$W1hiqualmum, df$W2hiqualmum, df$w4hiqualmum)
df$educpa_raw <- get_earliest_valid(df$W1hiqualdad, df$W2hiqualdad, df$w4hiqualdad)

# Map to 5-level
df$educma <- map_to_5level(df$educma_raw)
df$educpa <- map_to_5level(df$educpa_raw)

# Convert NA to -3 for 5-level variables
df$educma[is.na(df$educma)] <- -3
df$educpa[is.na(df$educpa)] <- -3

# Create labelled factors for detailed 20-category variables
level_labels_20 <- c(
  'Higher Degree', 'First Degree', 'HE Diploma', 'HNC/HND/NVQ4',
  'Teaching qualification, non-degree', 'Nursing qualification, non-degree',
  'A Levels', 'OND/ONC', 'City and guilds part III, NVQ3', 'CSYS',
  'Scottish Higher Grade', 'AS Level', 'Trade apprenticeship',
  'City and guilds part II, NVQ2', 'GCSE grade A-C and equivalent',
  'GCSE grade D-E and equivalent', 'City and guilds part I, NVQ1',
  'Youth training, skill seekers', 'Qualification, level unspecified',
  'No qualification mentioned'
)

df$educdtlma <- factor(df$educdtlma_raw, levels = c(1:20, -3), labels = c(level_labels_20, '-3'))
df$educdtlpa <- factor(df$educdtlpa_raw, levels = c(1:20, -3), labels = c(level_labels_20, '-3'))

# Create labelled factors for 5-level NVQ variables
level_labels_5 <- c(
  'Level 5 - Higher Degree',
  'Level 4 - Degree/Higher Diploma',
  'Level 3 - A-Level equivalent/Higher vocational',
  'Level 2 - GCSE equivalent/Intermediate vocational',
  'Level 1 - Lower qualification/None'
)

df$educma <- factor(df$educma, levels = c(1:5, -3), labels = c(level_labels_5, '-3'))
df$educpa <- factor(df$educpa, levels = c(1:5, -3), labels = c(level_labels_5, '-3'))

# Select final variables
output <- df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write output
cat('Writing output to data/output/cleaned_data.csv...\n')
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output dimensions:', dim(output), '\n')
cat('Output column names:', names(output), '\n')
cat('Done!\n')
