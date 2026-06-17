library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
wave1 <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c')) %>% select(NSID, W1englangYP)
wave2 <- readr::read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c')) %>% select(NSID, W2EnglangYP)
wave3 <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c')) %>% select(NSID, W3englangHH)
wave4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c')) %>% select(NSID, W4EngLangHH)

# Merge datasets
full_df <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Helper function to map missing values based on labels
map_missing <- function(val, labels) {
  if (is.na(val)) return(-3)
  val_num <- as.numeric(val)
  if (is.na(val_num)) return(-3)
  
  label <- labels[as.character(val_num)]
  if (is.na(label)) return(val_num)
  
  label <- tolower(label)
  if (grepl('refused', label)) return(-9)
  if (grepl('don\'t know|insufficient', label)) return(-8)
  if (grepl('prefer not to say', label)) return(-7)
  if (grepl('not interviewed|not asked', label)) return(-3)
  if (grepl('script error|information lost|missing|not applicable', label)) return(-2)
  if (grepl('not applicable', label)) return(-1)
  
  return(val_num)
}

# Process each wave variable
# Wave 1 labels
l1 <- c('-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know")
# Wave 2 labels
l2 <- c('-998.0' = 'Interviewer missed question', '-997.0' = 'Script error', '-995.0' = 'Missing history section data - unexplained', '-99.0' = 'YP not interviewed', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't Know")
# Wave 3 labels
l3 <- c('-999.0' = 'HH grid missing', '-997.0' = 'Script Error', '-99.0' = '', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know")
# Wave 4 labels
l4 <- c('-999.0' = 'Missing household grid', '-997.0' = 'Script error', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know")

# Harmonise each wave into a temporary column
clean_var <- function(col_name, labels) {
  sapply(full_df[[col_name]], function(x) map_missing(x, labels))
}

w1_clean <- clean_var('W1englangYP', l1)
w2_clean <- clean_var('W2EnglangYP', l2)
w3_clean <- clean_var('W3englangHH', l3)
w4_clean <- clean_var('W4EngLangHH', l4)

# Consolidated variable 'lang' (earliest-valid-first)
# Valid values are 1, 2, 3, 4
get_valid <- function(v) ifelse(v >= 1 & v <= 4, v, NA)

lang_v1 <- get_valid(w1_clean)
lang_v2 <- get_valid(w2_clean)
lang_v3 <- get_valid(w3_clean)
lang_v4 <- get_valid(w4_clean)

# Coalesce for earliest valid
lang_final <- coalesce(lang_v1, lang_v2, lang_v3, lang_v4)

# If no substantive response, find the most appropriate missing code (earliest missing)
# However, standard practice for consolidated is often to take the first available code if no substantive
missing_v1 <- w1_clean
missing_v2 <- w2_clean
missing_v3 <- w3_clean
missing_v4 <- w4_clean

lang_final <- ifelse(is.na(lang_final), coalesce(missing_v1, missing_v2, missing_v3, missing_v4), lang_final)
lang_final <- ifelse(is.na(lang_final), -3, lang_final)

# Create labelled factor
lang_labels <- c(
  `1` = 'Yes - English only',
  `2` = 'Yes - English first/ main and speaks other languages',
  `3` = 'No, another language is respondent\'s first or main language',
  `4` = 'Respondent is bilingual',
  `-9` = 'Refusal',
  `-8` = 'Don\'t know / insufficient information',
  `-7` = 'Prefer not to say',
  `-3` = 'Not asked at the fieldwork stage / not interviewed',
  `-2` = 'Schedule not applicable / script error / information lost',
  `-1` = 'Item not applicable'
)

final_df <- data.frame(NSID = full_df$NSID, lang = lang_final)
final_df$lang <- factor(final_df$lang, levels = names(lang_labels), labels = lang_labels)

# Write output
readr::write_csv(final_df, 'data/output/cleaned_data.csv')