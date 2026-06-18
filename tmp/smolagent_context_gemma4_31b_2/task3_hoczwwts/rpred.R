library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# File paths
files <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

# Load datasets
data_list <- lapply(files, function(f) {
  read_delim(paste0("data/input/", f), delim = "\t", col_types = cols(.default = "c"))
})

# Convert to data frames and ensure NSID is character
data_list <- lapply(data_list, function(df) {
  df %>% mutate(NSID = as.character(NSID))
})

# Give names to the list for easier access
names(data_list) <- files

# Extract and clean relevant variables
# Function to harmonize missing values based on general guidance and specific task requirements
harmonize_lang <- function(x) {
  x <- as.numeric(x)
  
  # Specific requirement: source -94 to -2
  # (Though -94 is not in the provided metadata, we include it for completeness as per instructions)
  x[x == -94] <- -2
  
  # Specific requirement: source -1 "Don't know" to -8
  x[x == -1] <- -8
  
  # General guidance mapping based on labels in metadata
  # -92.0 Refused -> -9
  x[x == -92] <- -9
  # -91.0 Not applicable -> -1
  x[x == -91] <- -1
  # -99.0 YP not interviewed / HH grid missing -> -3
  x[x == -99] <- -3
  # -999, -998, -997, -995 -> -2
  x[x %in% c(-999, -998, -997, -995)] <- -2
  
  # Convert NA to -3
  x[is.na(x)] <- -3
  
  return(x)
}

# Process each file to keep only NSID and the target variable
# Wave 1
df1 <- data_list[[1]] %>% select(NSID, W1englangYP) %>% 
  mutate(W1englangYP = harmonize_lang(W1englangYP))

# Wave 2
df2 <- data_list[[2]] %>% select(NSID, W2EnglangYP) %>% 
  mutate(W2EnglangYP = harmonize_lang(W2EnglangYP))

# Wave 3
df3 <- data_list[[3]] %>% select(NSID, W3englangHH) %>% 
  mutate(W3englangHH = harmonize_lang(W3englangHH))

# Wave 4
df4 <- data_list[[4]] %>% select(NSID, W4EngLangHH) %>% 
  mutate(W4EngLangHH = harmonize_lang(W4EngLangHH))

# Merge all
merged_df <- df1 %>%
  full_join(df2, by = "NSID") %>%
  full_join(df3, by = "NSID") %>%
  full_join(df4, by = "NSID")

# Consolidate: earliest valid positive response first (1-4)
# Logic: W1 -> W2 -> W3 -> W4
merged_df <- merged_df %>%
  mutate(lang = case_when(
    W1englangYP >= 1 & W1englangYP <= 4 ~ W1englangYP,
    W2EnglangYP >= 1 & W2EnglangYP <= 4 ~ W2EnglangYP,
    W3englangHH >= 1 & W3englangHH <= 4 ~ W3englangHH,
    W4EngLangHH >= 1 & W4EngLangHH <= 4 ~ W4EngLangHH,
    # Fallback to missing codes (earliest valid missing if no positive found)
    # The requirement says "use the earliest valid positive response first" 
    # and fallback through the list. If none are positive, we use the first available
    # value from the sequence of waves to determine the missing code.
    TRUE ~ coalesce(W1englangYP, W2EnglangYP, W3englangHH, W4EngLangHH, -3)
  ))

# Apply factor labels
lang_labels <- c(
  "1" = "Yes - English only",
  "2" = "Yes - English first/ main and speaks other languages",
  "3" = "No, another language is respondent's first or main language",
  "4" = "Respondent is bilingual",
  "-9" = "Refusal",
  "-8" = "Don't know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)

merged_df$lang <- factor(merged_df$lang, levels = as.numeric(names(lang_labels)), labels = lang_labels)

# Final selection
final_data <- merged_df %>% select(NSID, lang)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")