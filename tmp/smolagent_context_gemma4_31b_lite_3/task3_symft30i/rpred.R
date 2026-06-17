library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
# Using read_delim with delim = "\t". Removing the invalid 'q' shortcut.
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
file2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
file3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
file4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# 2. Merge datasets using full_join by NSID
merged_data <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file3, by = "NSID") %>%
  full_join(file4, by = "NSID")

# 3. Define a helper function to map missing values based on labels
map_missing <- function(val, wave_labels) {
  if (is.na(val)) return(-3)
  
  # Convert value to character to match metadata keys
  val_char <- as.character(round(as.numeric(val), 1))
  label <- wave_labels[val_char]
  
  if (is.na(label) || label == "") return(-3)
  
  label_lower <- tolower(label)
  
  if (grepl("refused", label_lower)) return(-9)
  if (grepl("don't know", label_lower)) return(-8)
  if (grepl("prefer not to say", label_lower)) return(-7)
  if (grepl("not interviewed", label_lower) || grepl("not asked", label_lower)) return(-3)
  if (grepl("script error", label_lower) || grepl("missing", label_lower) || grepl("information lost", label_lower)) return(-2)
  if (grepl("not applicable", label_lower)) return(-1)
  
  return(as.numeric(val))
}

# Labels from metadata (keys are strings representing the numeric values)
l1_labels <- c("-99" = "YP not interviewed", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "1" = "Yes - English only", "2" = "Yes - English first/ main and speaks other languages", "3" = "No, another language is respondent's first or main language", "4" = "Respondent is bilingual")
l2_labels <- c("-998" = "Interviewer missed question", "-997" = "Script error", "-995" = "Missing history section data - unexplained", "-99" = "YP not interviewed", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't Know", "1" = "Yes - English only", "2" = "Yes - English first/ main and speaks other languages", "3" = "No, another language is respondent's first or main language", "4" = "Respondent is bilingual")
l3_labels <- c("-999" = "HH grid missing", "-997" = "Script Error", "-99" = "", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "1" = "Yes - English only", "2" = "Yes - English first/main and speaks other languages", "3" = "No - another language is household's first/main language", "4" = "Household is bilingual")
l4_labels <- c("-999" = "Missing household grid", "-997" = "Script error", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "1" = "Yes - English only", "2" = "Yes - English first/main and speaks other languages", "3" = "No - another language is household's first/main language", "4" = "Household is bilingual")

# Cleaning function for a specific variable
clean_var <- function(vec, labels) {
  sapply(vec, function(x) map_missing(x, labels))
}

# Process and consolidate
lang_data <- merged_data %>%
  mutate(
    v1 = clean_var(W1englangYP, l1_labels),
    v2 = clean_var(W2EnglangYP, l2_labels),
    v3 = clean_var(W3englangHH, l3_labels),
    v4 = clean_var(W4EngLangHH, l4_labels)
  )

# Consolidation: Earliest-valid-first
get_consolidated <- function(row) {
  # 1. Look for substantive response (1-4)
  for (val in row) {
    if (!is.na(val) && val >= 1 && val <= 4) return(val)
  }
  # 2. Look for earliest missing code
  for (val in row) {
    if (!is.na(val)) return(val)
  }
  return(-3)
}

lang_final <- apply(lang_data[, c("v1", "v2", "v3", "v4")], 1, get_consolidated)

# Final dataframe
final_df <- data.frame(NSID = merged_data$NSID, lang = lang_final)

# Factor labels for 'lang'
lang_levels_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/main and speaks other languages",
  "3" = "No, another language is respondent's first or main language",
  "4" = "Respondent is bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

final_df$lang <- factor(final_df$lang, 
                        levels = as.numeric(names(lang_levels_labels)), 
                        labels = lang_levels_labels)

write_csv(final_df, "data/output/cleaned_data.csv")