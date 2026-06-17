library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols(.default = 'c'))

# Convert relevant variables to numeric
w1 <- w1 %>% mutate(W1englangYP = as.numeric(W1englangYP))
w2 <- w2 %>% mutate(W2EnglangYP = as.numeric(W2EnglangYP))
w3 <- w3 %>% mutate(W3englangHH = as.numeric(W3englangHH))
w4 <- w4 %>% mutate(W4EngLangHH = as.numeric(W4EngLangHH))

# Merge datasets
df <- w1 %>%
  full_join(w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

# Helper function for cleaning individual wave variables based on general guidance and specific requirements
clean_lang_var <- function(x) {
  # Map source -94 to -2 (as per additional requirements)
  # Note: Metadata doesn't explicitly show -94, but the requirement specifies it.
  x[x == -94] <- -2
  
  # Map source -1 labelled "Don't know" to -8 (as per additional requirements)
  x[x == -1] <- -8
  
  # Standard Missing-Value Codes (General Guidance)
  # -92 -> -9 (Refusal)
  x[x == -92] <- -9
  # -91 -> -1 (Not applicable)
  x[x == -91] <- -1
  # -99, -999, etc. -> -3 (Not asked / Not interviewed)
  x[x == -99 | x == -999 | x == -998 | x == -997 | x == -995] <- -3
  
  # Handle NAs as -3 per general guidance
  x[is.na(x)] <- -3
  
  return(x)
}

# Apply cleaning to source variables
df <- df %>%
  mutate(
    v1 = clean_lang_var(W1englangYP),
    v2 = clean_lang_var(W2EnglangYP),
    v3 = clean_lang_var(W3englangHH),
    v4 = clean_lang_var(W4EngLangHH)
  )

# Derivation logic: Earliest valid positive response first (1-4)
# If no positive response, fallback to the first available missing code in the priority order
df <- df %>%
  mutate(lang = case_when(
    v1 >= 1 & v1 <= 4 ~ v1,
    v2 >= 1 & v2 <= 4 ~ v2,
    v3 >= 1 & v3 <= 4 ~ v3,
    v4 >= 1 & v4 <= 4 ~ v4,
    # Fallback to missing codes in priority order
    v1 != -3 ~ v1,
    v2 != -3 ~ v2,
    v3 != -3 ~ v3,
    TRUE ~ v4
  ))

# Ensure final missing values are handled (NAs to -3)
df$lang[is.na(df$lang)] <- -3

# Define labels for the consolidated variable
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

df$lang <- factor(df$lang, levels = as.numeric(names(lang_labels)), labels = lang_labels)

# Final selection and output
final_df <- df %>%
  select(NSID, lang)

write_csv(final_df, 'data/output/cleaned_data.csv')