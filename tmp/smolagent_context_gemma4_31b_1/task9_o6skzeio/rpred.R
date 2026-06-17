library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load Files
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))

# Merge frames
full_frame <- w1 %>% 
  full_join(w2, by = 'NSID') %>% 
  full_join(w4, by = 'NSID')

# 2. Harmonise Missing Values
# Standard Scheme:
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / Not interviewed
# -2 = Schedule not applicable / script error / information lost
# -1 = Item not applicable

map_missing <- function(val) {
  if (is.na(val)) return(-3)
  # Based on metadata labels provided
  # -999.0: Missing - household data lost -> -2
  # -99.0: Not interviewed -> -3
  # -98.0: Not present -> -1 (Not applicable)
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

# Apply mapping to relevant columns
columns_to_clean <- c('W1hiqualmum', 'W1hiqualdad', 'W2hiqualmum', 'W2hiqualdad', 'w4hiqualmum', 'w4hiqualdad')
full_frame <- full_frame %>% 
  mutate(across(all_of(columns_to_clean), ~ sapply(.x, map_missing)))

# 3. Consolidation Logic
# Logic: scan waves 1, 2, 4 in order; first positive (1-20), then first negative code, then -3.

consolidate_edu <- function(w1v, w2v, w4v) {
  vals <- c(w1v, w2v, w4v)
  # First positive value
  pos <- vals[vals > 0 & !is.na(vals)]
  if (length(pos) > 0) return(pos[1])
  # First negative code
  neg <- vals[vals < 0 & !is.na(vals)]
  if (length(neg) > 0) return(neg[1])
  # Default
  return(-3)
}

full_frame <- full_frame %>% 
  rowwise() %>% 
  mutate(
    educdtlma = consolidate_edu(W1hiqualmum, W2hiqualmum, w4hiqualmum),
    educdtlpa = consolidate_edu(W1hiqualdad, W2hiqualdad, w4hiqualdad)
  ) %>% 
  ungroup()

# 4. Collapsed NVQ Scheme
# 0 = NVQ 4–5: Higher Degree (1), First Degree (2), HE Diploma (3), HNC/HND/NVQ4 (4)
# 1 = NVQ 1–3: Teaching (5), Nursing (6), A Levels (7), OND/ONC (8), C&G III (9), CSYS (10), Scot High (11), AS (12), Trade (13), C&G II (14), GCSE A-C (15), GCSE D-E (16), C&G I (17)
# 2 = Youth training / skill seekers (18)
# 3 = Qualification, level unspecified (19)
# 4 = No qualification mentioned (20)

map_nvq <- function(val) {
  if (val <= 0) return(val) # Preserve missing codes
  if (val >= 1 && val <= 4) return(0)
  if (val >= 5 && val <= 17) return(1)
  if (val == 18) return(2)
  if (val == 19) return(3)
  if (val == 20) return(4)
  return(val)
}

full_frame <- full_frame %>% 
  mutate(
    educma = sapply(educdtlma, map_nvq),
    educpa = sapply(educdtlpa, map_nvq)
  )

# 5. Final Formatting
# Labels for Detailed
detailed_labels <- c(
  '1' = 'Higher Degree', '2' = 'First Degree', '3' = 'HE Diploma', '4' = 'HNC/HND/NVQ4',
  '5' = 'Teaching qualification, non-degree', '6' = 'Nursing qualification, non-degree', '7' = 'A Levels', '8' = 'OND/ONC',
  '9' = 'City and guilds part III, NVQ3', '10' = 'CSYS', '11' = 'Scottish Higher Grade', '12' = 'AS Level',
  '13' = 'Trade apprenticeship', '14' = 'City and guilds part II, NVQ2', '15' = 'GCSE grade A-C and equivalent',
  '16' = 'GCSE grade D-E and equivalent', '17' = 'City and guilds part I, NVQ1', '18' = 'Youth training, skill seekers',
  '19' = 'Qualification, level unspecified', '20' = 'No qualification mentioned',
  '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed', '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable'
)

# Labels for Collapsed
nvq_labels <- c(
  '0' = 'NVQ 4–5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4',
  '1' = 'NVQ 1–3: everything from teaching/nursing qualifications through City & Guilds Part I / NVQ1',
  '2' = 'Youth training / skill seekers (training below NVQ level)',
  '3' = 'Qualification, level unspecified',
  '4' = 'No qualification mentioned',
  '-9' = 'Refusal', '-8' = 'Don\'t know / insufficient information', '-7' = 'Prefer not to say',
  '-3' = 'Not asked at the fieldwork stage / not interviewed', '-2' = 'Schedule not applicable / script error / information lost', '-1' = 'Item not applicable'
)

full_frame <- full_frame %>% 
  mutate(
    educdtlma = factor(educdtlma, levels = as.numeric(names(detailed_labels)), labels = detailed_labels),
    educdtlpa = factor(educdtlpa, levels = as.numeric(names(detailed_labels)), labels = detailed_labels),
    educma = factor(educma, levels = as.numeric(names(nvq_labels)), labels = nvq_labels),
    educpa = factor(educpa, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)
  )

# Keep only required variables
final_data <- full_frame %>% 
  select(NSID, educdtlma, educdtlpa, educma, educpa)

write_csv(final_data, 'data/output/cleaned_data.csv')