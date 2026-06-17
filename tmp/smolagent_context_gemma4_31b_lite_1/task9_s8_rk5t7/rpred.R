library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define the files to load
files <- c('wave_one_lsype_family_background_2020.tab', 
           'wave_two_lsype_family_background_2020.tab', 
           'wave_four_lsype_family_background_2020.tab')

# Load datasets
data_list <- lapply(files, function(f) {
  read_delim(paste0('data/input/', f), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
})

# Merge datasets
cohort_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  cohort_frame <- full_join(cohort_frame, data_list[[i]], by = 'NSID')
}

# Helper function for mapping missing values based on labels
map_missing <- function(x, labels) {
  # labels is a named vector where name is the original value and value is the label
  # We map by the label meaning
  res <- x
  
  # Define mapping logic based on General Guidance
  # -9 = Refusal
  # -8 = Don't know / insufficient information
  # -7 = Prefer not to say
  # -3 = Not asked / not interviewed
  # -2 = Schedule not applicable / script error / info lost
  # -1 = Item not applicable
  
  # Use a mapping table based on the metadata provided for these specific variables
  # Labels like 'Missing - household data lost', 'Mother not interviewed' etc.
  
  # We iterate through the values and check their labels
  unique_vals <- unique(x)
  for (val in unique_vals) {
    if (is.na(val)) next
    label <- labels[as.character(val)]
    if (is.na(label)) next
    
    if (grepl('household data lost|script error|information lost', label, ignore.case = TRUE)) {
      res[x == val] <- -2
    } else if (grepl('not interviewed|not asked', label, ignore.case = TRUE)) {
      res[x == val] <- -3
    } else if (grepl('not present', label, ignore.case = TRUE)) {
      res[x == val] <- -3
    } else if (grepl('Insufficient information', label, ignore.case = TRUE)) {
      res[x == val] <- -8
    } else if (grepl('Refused', label, ignore.case = TRUE)) {
      res[x == val] <- -9
    } else if (grepl('Not applicable', label, ignore.case = TRUE)) {
      res[x == val] <- -1
    } else if (grepl('Don\'t know', label, ignore.case = TRUE)) {
      res[x == val] <- -8
    }
  }
  
  # Convert R NA to -3
  res[is.na(res)] <- -3
  return(res)
}

# Define labels for the variables (extracted from metadata)
labels_mum <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable')
labels_dad <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = 'Don\'t know')

# Process Mother's variables
cohort_frame <- cohort_frame %>%
  mutate(
    m1 = map_missing(W1hiqualmum, labels_mum),
    m2 = map_missing(W2hiqualmum, labels_mum),
    m4 = map_missing(w4hiqualmum, labels_mum)
  )

# Process Father's variables
cohort_frame <- cohort_frame %>%
  mutate(
    d1 = map_missing(W1hiqualdad, labels_dad),
    d2 = map_missing(W2hiqualdad, labels_dad),
    d4 = map_missing(w4hiqualdad, labels_dad)
  )

# Consolidation: Earliest-valid-first
consolidate_earliest <- function(v1, v2, v3) {
  res <- v1
  res[res < 0] <- v2[res < 0]
  res[res < 0] <- v3[res < 0]
  return(res)
}

cohort_frame <- cohort_frame %>%
  mutate(
    educdtlma = consolidate_earliest(m1, m2, m4),
    educdtlpa = consolidate_earliest(d1, d2, d4)
  )

# Harmonisation to 5-level NVQ
# Based on standard UK NVQ levels usually: 
# 1: Higher Degree/First Degree/HE Diploma/NVQ4 (Levels 4+)
# 2: A-Levels/HNC/HND (Levels 3)
# 3: NVQ3/City & Guilds III (Levels 2/3)
# 4: GCSE/NVQ2 (Levels 1/2)
# 5: No qualification

recode_nvq <- function(x) {
  res <- x
  # Level 1: Higher Education (1, 2, 3, 4)
  res[x %in% c(1, 2, 3, 4)] <- 1
  # Level 2: A-Levels / HNC / HND / Scottish Higher (7, 8, 11, 12)
  res[x %in% c(7, 8, 11, 12)] <- 2
  # Level 3: NVQ3 / Trade / CSYS (9, 10, 13)
  res[x %in% c(9, 10, 13)] <- 3
  # Level 4: GCSE / NVQ2 / Youth Training (14, 15, 16, 17, 18)
  res[x %in% c(14, 15, 16, 17, 18)] <- 4
  # Level 5: No qualification / Unspecified (19, 20)
  res[x %in% c(19, 20)] <- 5
  
  # Preserve missing codes
  missing_mask <- x < 0
  res[missing_mask] <- x[missing_mask]
  
  return(res)
}

cohort_frame <- cohort_frame %>%
  mutate(
    educma = recode_nvq(educdtlma),
    educpa = recode_nvq(educdtlpa)
  )

# Create factors with labels
# Detailed labels (20 categories + missing)
detailed_labels <- c(
  '-9' = 'Refusal', '-8' = 'Don\'t know', '-7' = 'Prefer not to say', 
  '-3' = 'Not interviewed', '-2' = 'Information lost', '-1' = 'Not applicable',
  '1' = 'Higher Degree', '2' = 'First Degree', '3' = 'HE Diploma', '4' = 'HNC/HND/NVQ4',
  '5' = 'Teaching qualification, non-degree', '6' = 'Nursing qualification, non-degree', 
  '7' = 'A Levels', '8' = 'OND/ONC', '9' = 'City and guilds part III, NVQ3', 
  '10' = 'CSYS', '11' = 'Scottish Higher Grade', '12' = 'AS Level', 
  '13' = 'Trade apprenticeship', '14' = 'City and guilds part II, NVQ2', 
  '15' = 'GCSE grade A-C and equivalent', '16' = 'GCSE grade D-E and equivalent', 
  '17' = 'City and guilds part I, NVQ1', '18' = 'Youth training, skill seekers', 
  '19' = 'Qualification, level unspecified', '20' = 'No qualification mentioned'
)

# NVQ labels
nvq_labels <- c(
  '-9' = 'Refusal', '-8' = 'Don\'t know', '-7' = 'Prefer not to say', 
  '-3' = 'Not interviewed', '-2' = 'Information lost', '-1' = 'Not applicable',
  '1' = 'Higher Education', '2' = 'A-Level equivalent', '3' = 'NVQ3 equivalent', 
  '4' = 'GCSE equivalent', '5' = 'No qualification'
)

cohort_frame <- cohort_frame %>%
  mutate(
    educdtlma = factor(educdtlma, levels = as.numeric(names(detailed_labels)), labels = detailed_labels),
    educdtlpa = factor(educdtlpa, levels = as.numeric(names(detailed_labels)), labels = detailed_labels),
    educma = factor(educma, levels = as.numeric(names(nvq_labels)), labels = nvq_labels),
    educpa = factor(educpa, levels = as.numeric(names(nvq_labels)), labels = nvq_labels)
  )

# Final selection
final_data <- cohort_frame %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

write_csv(final_data, 'data/output/cleaned_data.csv')
