library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define filenames from metadata
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab'
)

# Load files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Convert relevant columns to numeric for processing
w1$W1englangYP <- as.numeric(w1$W1englangYP)
w2$W2EnglangYP <- as.numeric(w2$W2EnglangYP)
w3$W3englangHH <- as.numeric(w3$W3englangHH)
w4$W4EngLangHH <- as.numeric(w4$W4EngLangHH)

# Merge datasets
df <- w1 %>% 
  full_join(w2, by = 'NSID') %>% 
  full_join(w3, by = 'NSID') %>% 
  full_join(w4, by = 'NSID')

# 6 & 7. Missing Value Harmonisation Function
harmonise_missing <- function(x, labels_map) {
  # Map based on label meanings provided in metadata
  # -9 = Refusal, -8 = Don't know, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule not applicable/Script error, -1 = Not applicable
  
  res <- x
  # The labels_map is a named vector where name is raw value and value is standard code
  for (val in names(labels_map)) {
    res[x == as.numeric(val)] <- labels_map[[val]]
  }
  
  # Convert NA to -3 (Not asked/Not interviewed)
  res[is.na(res)] <- -3
  return(res)
}

# Define specific maps based on metadata labels
map_w1 <- c("-99.0" = -3, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8)
map_w2 <- c("-998.0" = -2, "-997.0" = -2, "-995.0" = -2, "-99.0" = -3, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8)
map_w3 <- c("-999.0" = -2, "-997.0" = -2, "-99.0" = -3, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8)
map_w4 <- c("-999.0" = -2, "-997.0" = -2, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8)

# Apply harmonisation
df$lang_w1 <- harmonise_missing(df$W1englangYP, map_w1)
df$lang_w2 <- harmonise_missing(df$W2EnglangYP, map_w2)
df$lang_w3 <- harmonise_missing(df$W3englangHH, map_w3)
df$lang_w4 <- harmonise_missing(df$W4EngLangHH, map_w4)

# 8. Response Category Harmonisation
# Categories are identical across all 4 waves:
# 1: Yes - English only
# 2: Yes - English first/main and speaks other languages
# 3: No - another language is first/main
# 4: Bilingual
# No collapsing needed as categories align.

# 9. Consolidation (Earliest-Valid-First)
# Order: W1 -> W2 -> W3 -> W4
get_first_valid <- function(row) {
  vals <- row
  # Valid substantive responses are 1, 2, 3, 4
  for (v in vals) {
    if (!is.na(v) && v >= 1 && v <= 4) return(v)
  }
  # Fallback to missing codes in order of appearance
  for (v in vals) {
    if (!is.na(v)) return(v)
  }
  return(-3)
}

# Apply consolidation
df$lang <- apply(df[, c('lang_w1', 'lang_w2', 'lang_w3', 'lang_w4')], 1, get_first_valid)

# 10. Labels and Data Types
lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No - another language is first/main language",
  "4" = "Bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know",
  "-7" = "Prefer not to say",
  "-3" = "Not asked",
  "-2" = "Schedule not applicable",
  "-1" = "Not applicable"
)

df$lang <- factor(df$lang, levels = c(1, 2, 3, 4, -9, -8, -7, -3, -2, -1), 
                 labels = lang_labels[as.character(c(1, 2, 3, 4, -9, -8, -7, -3, -2, -1))])

# 12. Output Requirements
final_df <- df %>% select(NSID, lang)
write_csv(final_df, 'data/output/cleaned_data.csv')