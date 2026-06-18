library(readr)
library(dplyr)
library(haven)

# Harmonise missing codes to standard scheme
harmonise_missing <- function(x) {
  case_when(
    x == -999 ~ -2,      # Missing – household data lost
    x == -99  ~ -3,      # Not interviewed
    x == -98  ~ -3,      # Not present
    x == -94  ~ -8,      # Insufficient information
    x == -92  ~ -9,      # Refused
    x == -91  ~ -1,      # Not applicable
    x == -1   ~ -8,      # Don’t know
    is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Collapse detailed education into NVQ levels
collapse_nvq <- function(x) {
  case_when(
    x >= 1 & x <= 4 ~ 0,   # Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4
    x >= 5 & x <= 17 ~ 1,  # Teaching/Nursing etc. up to NVQ1
    x == 18 ~ 2,           # Youth training / skill seekers
    x == 19 ~ 3,           # Qualification, level unspecified
    x == 20 ~ 4,           # No qualification mentioned
    is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Read wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = col_guess(), NSID = col_character()))

# Keep only relevant columns
wave1_sel <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_sel <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_sel <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge all waves
merged_df <- wave1_sel %>% full_join(wave2_sel, by = "NSID") %>% full_join(wave4_sel, by = "NSID")

# Harmonise missing codes for all qualification variables
merged_df <- merged_df %>% mutate(
  across(c(W1hiqualmum, W1hiqualdad, W2hiqualmum, W2hiqualdad, w4hiqualmum, w4hiqualdad), ~harmonise_missing(.))
)

# Consolidate into detailed parental education variables
merged_df <- merged_df %>% mutate(
  educdtlma = case_when(
    W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ W1hiqualmum,
    W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ W2hiqualmum,
    w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ w4hiqualmum,
    W1hiqualmum < 0 ~ W1hiqualmum,
    W2hiqualmum < 0 ~ W2hiqualmum,
    w4hiqualmum < 0 ~ w4hiqualmum,
    TRUE ~ -3
  ),
  educdtlpa = case_when(
    W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ W1hiqualdad,
    W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ W2hiqualdad,
    w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ w4hiqualdad,
    W1hiqualdad < 0 ~ W1hiqualdad,
    W2hiqualdad < 0 ~ W2hiqualdad,
    w4hiqualdad < 0 ~ w4hiqualdad,
    TRUE ~ -3
  )
)

# Collapse into NVQ levels
merged_df <- merged_df %>% mutate(
  educma = collapse_nvq(educdtlma),
  educpa = collapse_nvq(educdtlpa)
)

# Define labelled vectors: names = labels, values = numeric codes
label_detailed <- c(
  "Refusal" = -9,
  "Don\'t know / insufficient information" = -8,
  "Prefer not to say" = -7,
  "Not asked at fieldwork stage / not interviewed" = -3,
  "Schedule not applicable / script error / information lost" = -2,
  "Item not applicable" = -1,
  "Higher Degree" = 1,
  "First Degree" = 2,
  "HE Diploma" = 3,
  "HNC/HND/NVQ4" = 4,
  "Teaching qualification, non-degree" = 5,
  "Nursing qualification, non-degree" = 6,
  "A Levels" = 7,
  "OND/ONC" = 8,
  "City and guilds part III, NVQ3" = 9,
  "CSYS" = 10,
  "Scottish Higher Grade" = 11,
  "AS Level" = 12,
  "Trade apprenticeship" = 13,
  "City and guilds part II, NVQ2" = 14,
  "GCSE grade A-C and equivalent" = 15,
  "GCSE grade D-E and equivalent" = 16,
  "City and guilds part I, NVQ1" = 17,
  "Youth training, skill seekers" = 18,
  "Qualification, level unspecified" = 19,
  "No qualification mentioned" = 20
)

label_collapsed <- c(
  "Refusal" = -9,
  "Don\'t know / insufficient information" = -8,
  "Prefer not to say" = -7,
  "Not asked at fieldwork stage / not interviewed" = -3,
  "Schedule not applicable / script error / information lost" = -2,
  "Item not applicable" = -1,
  "NVQ 4–5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4" = 0,
  "NVQ 1–3: everything from teaching/nursing qualifications through City & Guilds Part I / NVQ1" = 1,
  "Youth training / skill seekers (training below NVQ level)" = 2,
  "Qualification, level unspecified" = 3,
  "No qualification mentioned" = 4
)

# Apply labels
merged_df <- merged_df %>% mutate(
  educdtlma = labelled(educdtlma, label_detailed),
  educdtlpa = labelled(educdtlpa, label_detailed),
  educma    = labelled(educma,    label_collapsed),
  educpa    = labelled(educpa,    label_collapsed)
)

# Keep final variables
final_df <- merged_df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write CSV
write_csv(final_df, "data/output/cleaned_data.csv")
