library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
file1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())
file4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = readr::cols())

df <- file1 %>% 
  full_join(file2, by = "NSID") %>% 
  full_join(file4, by = "NSID")

# 6. Missing Value Code Harmonization Helper
harmonize_missing <- function(val) {
  if (is.na(val)) return(-3)
  if (val == -999) return(-2)
  if (val == -99) return(-3)
  if (val == -98) return(-3)
  if (val == -94) return(-8)
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val == -1) return(-8)
  return(val)
}

apply_harm <- function(x) { sapply(x, harmonize_missing) }

df <- df %>%
  mutate(
    W1m_h = apply_harm(W1hiqualmum),
    W1f_h = apply_harm(W1hiqualdad),
    W2m_h = apply_harm(W2hiqualmum),
    W2f_h = apply_harm(W2hiqualdad),
    W4m_h = apply_harm(w4hiqualmum),
    W4f_h = apply_harm(w4hiqualdad)
  )

# 4. Consolidation Logic
consolidate_edu <- function(w1, w2, w4) {
  res <- case_when(
    w1 > 0 ~ w1,
    w2 > 0 ~ w2,
    w4 > 0 ~ w4,
    TRUE ~ coalesce(w1, w2, w4)
  )
  res[is.na(res)] <- -3
  return(res)
}

df <- df %>%
  mutate(
    educdtlma = consolidate_edu(W1m_h, W2m_h, W4m_h),
    educdtlpa = consolidate_edu(W1f_h, W2f_h, W4f_h)
  )

# 5. Derived Collapsed Variables
collapse_edu <- function(detailed) {
  case_when(
    detailed >= 1 & detailed <= 4 ~ 0,
    detailed >= 5 & detailed <= 17 ~ 1,
    detailed == 18 ~ 2,
    detailed == 19 ~ 3,
    detailed == 20 ~ 4,
    detailed < 0 ~ detailed,
    TRUE ~ -3
  )
}

df <- df %>%
  mutate(
    educma = collapse_edu(educdtlma),
    educpa = collapse_edu(educdtlpa)
  )

# 7. Factor Variables and Labels
# Using factor() instead of as_factor() to avoid the error and correctly apply labels
detail_labels <- c(
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree",
  "7" = "A Levels", "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS", "11" = "Scottish Higher Grade", "12" = "AS Level",
  "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1", "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified", "20" = "No qualification mentioned"
)

collapsed_labels <- c(
  "-9" = "Refusal", "-8" = "Don't know/insufficient information",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "0" = "NVQ 4-5", "1" = "NVQ 1-3", "2" = "None/entry", "3" = "Other", "4" = "No qualifications"
)

# Function to safely convert to factor with specific labels
apply_labels <- function(x, label_vec) {
  # Convert x to character to match names of label_vec
  x_char <- as.character(x)
  # Map labels
  labels <- label_vec[x_char]
  # Create factor with levels from the label_vec
  factor(labels, levels = unname(label_vec))
}

df <- df %>%
  mutate(
    educdtlma = apply_labels(educdtlma, detail_labels),
    educdtlpa = apply_labels(educdtlpa, detail_labels),
    educma = apply_labels(educma, collapsed_labels),
    educpa = apply_labels(educpa, collapsed_labels)
  )

# 10. Output Requirements
final_df <- df %>% select(NSID, educma, educpa, educdtlma, educdtlpa)
write_csv(final_df, "data/output/cleaned_data.csv")
