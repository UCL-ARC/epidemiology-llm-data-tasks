library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
# Files listed in metadata
files <- c('wave_one_lsype_family_background_2020.tab', 
            'wave_two_lsype_family_background_2020.tab', 
            'wave_four_lsype_family_background_2020.tab')

load_tab <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = cols(.default = 'numeric'))
}

# Note: NSID is string, so we need to ensure it's read correctly. 
# Since the metadata says NSID is string, let's explicitly handle it.
read_tab_custom <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = cols(NSID = col_character(), .default = 'numeric'))
}

w1 <- read_tab_custom('wave_one_lsype_family_background_2020.tab')
w2 <- read_tab_custom('wave_two_lsype_family_background_2020.tab')
w4 <- read_tab_custom('wave_four_lsype_family_background_2020.tab')

# 2. Merge datasets
full_frame <- w1 %>% 
  full_join(w2, by = 'NSID') %>% 
  full_join(w4, by = 'NSID')

# 3. Define Missing Value Mapping
# Standard: -9 Refusal, -8 Don't know, -7 Prefer not to say, -3 Not asked/NA, -2 Schedule not applicable/lost, -1 Not applicable

map_missing <- function(val, labels) {
  if (is.na(val)) return(-3)
  
  # Check label meanings based on metadata
  # W1/W2/W4 common patterns
  # -999.0: Missing - household data lost -> -2
  # -99.0: Mother/Father not interviewed -> -3
  # -98.0: Mother/Father not present -> -1
  # -94.0: Insufficient information -> -8
  # -92.0: Refused -> -9
  # -91.0: Not applicable -> -1
  # -1.0: Don't know -> -8
  
  if (val == -999) return(-2)
  if (val == -99) return(-3)
  if (val == -98) return(-1)
  if (val == -94) return(-8)
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val == -1) return(-8)
  
  return(val)
}

# Apply mapping to the source variables
sources_mum <- c('W1hiqualmum', 'W2hiqualmum', 'w4hiqualmum')
sources_dad <- c('W1hiqualdad', 'W2hiqualdad', 'w4hiqualdad')

# Process Mother's Education
process_edu <- function(ids, source_vars) {
  # Create a matrix-like structure for consolidation
  res <- apply(full_frame[, source_vars], 1, function(row) {
    # Map missing values first
    mapped_row <- sapply(row, function(x) map_missing(x, NULL))
    
    # Logic: scan waves (1, 2, 4) in order
    # 1. First positive value (1-20)
    for (val in mapped_row) {
      if (!is.na(val) && val > 0) return(val)
    }
    # 2. First negative code
    for (val in mapped_row) {
      if (!is.na(val) && val < 0) return(val)
    }
    # 3. Fallback -3
    return(-3)
  })
  return(res)
}

full_frame$educdtlma <- process_edu(full_frame$NSID, sources_mum)
full_frame$educdtlpa <- process_edu(full_frame$NSID, sources_dad)

# 4. Derive Collapsed NVQ Scheme
# 0 = NVQ 4–5: 1, 2, 3, 4
# 1 = NVQ 1–3: 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17
# 2 = Youth training / skill seekers: 18
# 3 = Qualification, level unspecified: 19
# 4 = No qualification mentioned: 20

collapse_nvq <- function(detailed_val) {
  if (is.na(detailed_val)) return(-3)
  if (detailed_val < 0) return(detailed_val)
  
  if (detailed_val >= 1 && detailed_val <= 4) return(0)
  if (detailed_val >= 5 && detailed_val <= 17) return(1)
  if (detailed_val == 18) return(2)
  if (detailed_val == 19) return(3)
  if (detailed_val == 20) return(4)
  
  return(-3)
}

full_frame$educma <- sapply(full_frame$educdtlma, collapse_nvq)
full_frame$educpa <- sapply(full_frame$educdtlpa, collapse_nvq)

# 5. Factor Labels
# Detailed labels (1-20)
detailed_labels <- c(
  '1' = 'Higher Degree', '2' = 'First Degree', '3' = 'HE Diploma', '4' = 'HNC/HND/NVQ4',
  '5' = 'Teaching qualification, non-degree', '6' = 'Nursing qualification, non-degree',
  '7' = 'A Levels', '8' = 'OND/ONC', '9' = 'City and guilds part III, NVQ3', '10' = 'CSYS',
  '11' = 'Scottish Higher Grade', '12' = 'AS Level', '13' = 'Trade apprenticeship',
  '14' = 'City and guilds part II, NVQ2', '15' = 'GCSE grade A-C and equivalent',
  '16' = 'GCSE grade D-E and equivalent', '17' = 'City and guilds part I, NVQ1',
  '18' = 'Youth training, skill seekers', '19' = 'Qualification, level unspecified',
  '20' = 'No qualification mentioned',
  '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed', 
  '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable'
)

# NVQ labels
nvq_labels <- c(
  '0' = 'NVQ 4–5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4',
  '1' = 'NVQ 1–3: everything from teaching/nursing qualifications through City & Guilds Part I / NVQ1',
  '2' = 'Youth training / skill seekers (training below NVQ level)',
  '3' = 'Qualification, level unspecified',
  '4' = 'No qualification mentioned',
  '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed', 
  '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable'
)

full_frame$educdtlma <- factor(full_frame$educdtlma, levels = as.numeric(names(detailed_labels)), labels = detailed_labels)
full_frame$educdtlpa <- factor(full_frame$educdtlpa, levels = as.numeric(names(detailed_labels)), labels = detailed_labels)
full_frame$educma <- factor(full_frame$educma, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)
full_frame$educpa <- factor(full_frame$educpa, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)

# 6. Final output
final_data <- full_frame %>% 
  select(NSID, educdtlma, educdtlpa, educma, educpa)

write_csv(final_data, 'data/output/cleaned_data.csv')
