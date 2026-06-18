library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

input_dir <- "data/input"
output_file <- "data/output/cleaned_data.csv"

# File names
files <- list(
  wave1 = "wave_one_lsype_young_person_2020.tab",
  wave2 = "wave_two_lsype_young_person_2020.tab",
  wave3 = "wave_three_lsype_family_background_2020.tab",
  wave4 = "wave_four_lsype_family_background_2020.tab"
)

# Read each file
wave1 <- read_delim(file.path(input_dir, files$wave1), delim = "\t", col_types = cols(), show_col_types = FALSE)
wave2 <- read_delim(file.path(input_dir, files$wave2), delim = "\t", col_types = cols(), show_col_types = FALSE)
wave3 <- read_delim(file.path(input_dir, files$wave3), delim = "\t", col_types = cols(), show_col_types = FALSE)
wave4 <- read_delim(file.path(input_dir, files$wave4), delim = "\t", col_types = cols(), show_col_types = FALSE)

# Function to map language codes to harmonised codes
map_lang_vec <- function(v) {
  sapply(v, function(x) {
    if (is.na(x)) return(-3L)
    # Negative codes handling
    if (x == -92) return(-9L)          # Refused
    if (x == -91) return(-1L)          # Not applicable
    if (x == -1)  return(-8L)          # Don’t know / insufficient info
    if (x == -99) return(-3L)          # Not interviewed
    if (x %in% c(-999, -998, -997, -995)) return(-2L)  # Schedule not applicable / script error / info lost
    # Positive substantive values
    if (x %in% c(1, 2, 3, 4)) return(as.integer(x))
    # Any other code treated as missing
    return(-3L)
  })
}

# Apply mapping to each wave variable
wave1 <- wave1 %>% mutate(lang14 = map_lang_vec(W1englangYP))
wave2 <- wave2 %>% mutate(lang15 = map_lang_vec(W2EnglangYP))
wave3 <- wave3 %>% mutate(lang16 = map_lang_vec(W3englangHH))
wave4 <- wave4 %>% mutate(lang17 = map_lang_vec(W4EngLangHH))

# Merge all waves by NSID
merged <- wave1 %>%
  full_join(wave2 %>% select(NSID, lang15), by = "NSID") %>%
  full_join(wave3 %>% select(NSID, lang16), by = "NSID") %>%
  full_join(wave4 %>% select(NSID, lang17), by = "NSID")

# Consolidate into one language variable (earliest substantive response)
consolidate_lang <- function(langs) {
  for (val in langs) {
    if (val %in% c(1L, 2L, 3L, 4L)) return(val)
  }
  for (val in langs) {
    if (!is.na(val) && val %in% c(-9L, -8L, -1L, -3L, -2L)) return(val)
  }
  return(-3L)
}

merged <- merged %>% rowwise() %>% mutate(lang = consolidate_lang(c_across(starts_with("lang")))) %>% ungroup()

# Keep only NSID and consolidated variable
final_df <- merged %>% select(NSID, lang)

# Write output CSV
write_csv(final_df, output_file)
