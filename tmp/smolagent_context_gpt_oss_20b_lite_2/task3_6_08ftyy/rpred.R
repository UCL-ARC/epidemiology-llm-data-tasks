library(readr)
library(dplyr)
library(labelled)
library(haven)

# Harmonise language codes across waves
harmonise_lang <- function(x) {
  x_mapped <- case_when(
    x %in% c(-999, -998, -997, -995) ~ -2,  # schedule not applicable / script error
    x == -99 ~ -3,                          # not interviewed
    x == -92 ~ -9,                          # refused
    x == -91 ~ -1,                          # item not applicable
    x == -1  ~ -8,                          # don\'t know
    TRUE ~ x
  )
  x_mapped[is.na(x_mapped)] <- -3            # convert NA to ‘not asked’
  # collapse detailed categories
  x_final <- case_when(
    x_mapped == 1 ~ 1,
    x_mapped == 2 ~ 2,
    x_mapped == 3 ~ 3,
    x_mapped == 4 ~ 2,                     # treat bilingual as 2
    TRUE ~ x_mapped
  )
  return(x_final)
}

# Read a wave file and rename the language variable
read_wave <- function(path, var_name, new_name) {
  df <- read_delim(path, delim = "\t", col_types = cols(), show_col_types = FALSE)
  df %>% rename(!!new_name := !!var_name)
}

# Load all waves
w1 <- read_wave("data/input/wave_one_lsype_young_person_2020.tab", "W1englangYP", "w1_lang")
w2 <- read_wave("data/input/wave_two_lsype_young_person_2020.tab", "W2EnglangYP", "w2_lang")
w3 <- read_wave("data/input/wave_three_lsype_family_background_2020.tab", "W3englangHH", "w3_lang")
w4 <- read_wave("data/input/wave_four_lsype_family_background_2020.tab", "W4EngLangHH", "w4_lang")

# Merge all waves by NSID
merged <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Harmonise each language variable
merged <- merged %>% mutate(
  w1_lang_h = harmonise_lang(w1_lang),
  w2_lang_h = harmonise_lang(w2_lang),
  w3_lang_h = harmonise_lang(w3_lang),
  w4_lang_h = harmonise_lang(w4_lang)
)

# Consolidated language variable – earliest valid answer
merged <- merged %>% mutate(lang = coalesce(w1_lang_h, w2_lang_h, w3_lang_h, w4_lang_h))

# Value labels for the consolidated variable
lang_labels <- c(
  "English only" = 1,
  "Bilingual (English & other)" = 2,
  "Non-English first/main" = 3,
  "Item not applicable" = -1,
  "Schedule not applicable / script error / information lost" = -2,
  "Not asked / not interviewed" = -3,
  "Don\'t know" = -8,
  "Refused" = -9
)

merged$lang <- haven::labelled(merged$lang, labels = lang_labels)

# Keep only the required variables
final_df <- merged %>% select(NSID, lang)

# Ensure output directory exists
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

write_csv(final_df, "data/output/cleaned_data.csv")
cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")
