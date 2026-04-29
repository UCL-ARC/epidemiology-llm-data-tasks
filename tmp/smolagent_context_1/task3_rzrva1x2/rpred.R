library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define files from metadata
files_metadata <- list(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

# Read files and merge
full_cohort <- files_metadata %>%
  map(~ read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = "c"))) %>%
  reduce(full_join, by = 'NSID')

# Convert target variables to numeric for processing
target_vars <- c('W1englangYP', 'W2EnglangYP', 'W3englangHH', 'W4EngLangHH')
full_cohort[target_vars] <- map(full_cohort[target_vars], as.numeric)

# 6 & 7. Missing Value Harmonisation Function
harmonise_missing <- function(x, var_name) {
  # Standard missing-value codes:
  # -9 = Refusal
  # -8 = Don't know / insufficient information
  # -7 = Prefer not to say
  # -3 = Not asked at the fieldwork stage / not interviewed
  # -2 = Schedule not applicable / script error / information lost
  # -1 = Item not applicable
  
  # Specific task mappings
  # -94 -> -2
  # -1 (Don't know) -> -8
  
  res <- x
  
  # General mappings based on labels/common defaults
  res[x == -99] <- -3 # Not interviewed
  res[x == -92] <- -9 # Refused
  res[x == -91] <- -1 # Not applicable
  
  # Wave specific / Common Defaults for script errors/lost data
  res[x %in% c(-999, -998, -997, -995)] <- -2
  res[x == -94] <- -2
  
  # Specific requirement: -1 (Don't know) -> -8
  res[x == -1] <- -8
  
  # Final NA to -3
  res[is.na(res)] <- -3
  
  return(res)
}

# Apply harmonisation to each wave
full_cohort <- full_cohort %>%
  mutate(
    eng_14 = harmonise_missing(W1englangYP, 'W1englangYP'),
    eng_15 = harmonise_missing(W2EnglangYP, 'W2EnglangYP'),
    eng_16 = harmonise_missing(W3englangHH, 'W3englangHH'),
    eng_17 = harmonise_missing(W4EngLangHH, 'W4EngLangHH')
  )

# 8. Response Category Harmonisation
# All variables follow the same 1-4 scale for substantive responses:
# 1: English only
# 2: English first/main + others
# 3: Other first/main
# 4: Bilingual
# Since they are already aligned in the metadata, we keep them as is.

# 9. Consolidation Logic: Earliest-positive-first, then earliest-negative-first
# Construct: English main language (consolidated)

consolidate_eng <- function(row) {
  vals <- c(row[['eng_14']], row[['eng_15']], row[['eng_16']], row[['eng_17']])
  
  # Earliest positive (substantive response > 0)
  pos_idx <- which(vals > 0)[1]
  if (!is.na(pos_idx)) return(vals[pos_idx])
  
  # Earliest negative (missing code < 0)
  neg_idx <- which(vals < 0)[1]
  if (!is.na(neg_idx)) return(vals[neg_idx])
  
  return(-3) # Default fallback
}

full_cohort <- full_cohort %>%
  rowwise() %>%
  mutate(eng_main = consolidate_eng(cur_data())) %>%
  ungroup()

# 10. Labels and Data Types
eng_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No, another language is first/main language",
  "4" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

# Apply labels to consolidated and wave variables
full_cohort <- full_cohort %>%
  mutate(
    eng_main = factor(eng_main, levels = as.numeric(names(eng_labels)), labels = eng_labels),
    eng_14 = factor(eng_14, levels = as.numeric(names(eng_labels)), labels = eng_labels),
    eng_15 = factor(eng_15, levels = as.numeric(names(eng_labels)), labels = eng_labels),
    eng_16 = factor(eng_16, levels = as.numeric(names(eng_labels)), labels = eng_labels),
    eng_17 = factor(eng_17, levels = as.numeric(names(eng_labels)), labels = eng_labels)
  )

# 12. Output Requirements
final_output <- full_cohort %>%
  select(NSID, eng_main, eng_14, eng_15, eng_16, eng_17)

write_csv(final_output, 'data/output/cleaned_data.csv')