library(readr)
library(dplyr)
library(labelled)

# Helper to map source‑specific missing codes to the standard scheme
map_missing <- function(x) {
  ifelse(is.na(x), -3,
         ifelse(x == -999, -2,
                ifelse(x %in% c(-99, -98), -3,
                       ifelse(x == -94, -8,
                              ifelse(x == -92, -9,
                                     ifelse(x == -91, -1,
                                            ifelse(x == -1, -8, x)))))))
}

# Load the three sweep files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab",
                 delim = "\t", escape_double = FALSE, trim_ws = TRUE)

w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab",
                 delim = "\t", escape_double = FALSE, trim_ws = TRUE)

w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab",
                 delim = "\t", escape_double = FALSE, trim_ws = TRUE)

# Standardise missing codes within each sweep
w1 <- w1 %>%
  mutate(
    w1_hq_mum = map_missing(W1hiqualmum),
    w1_hq_dad = map_missing(W1hiqualdad)
  )

w2 <- w2 %>%
  mutate(
    w2_hq_mum = map_missing(W2hiqualmum),
    w2_hq_dad = map_missing(W2hiqualdad)
  )

w4 <- w4 %>%
  mutate(
    w4_hq_mum = map_missing(w4hiqualmum),
    w4_hq_dad = map_missing(w4hiqualdad)
  )

# Merge all sweeps on the cohort ID (NSID)
merged <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Collapse the detailed qualification codes to a 20‑category NVQ scheme
collapse_nvq <- function(dtl) {
  case_when(
    dtl == 17 ~ 1,   # NVQ1
    dtl == 14 ~ 2,   # NVQ2
    dtl == 9  ~ 3,   # NVQ3
    dtl == 4  ~ 4,   # NVQ4
    dtl %in% c(1,2,3,5,6,7,8,10,11,12,13,15,16,19,20) ~ 5,
    TRUE ~ NA_real_
  )
}

# Create consolidated detailed variables (mother & father)
merged <- merged %>%
  mutate(
    educdtlma_raw = coalesce(w1_hq_mum, w2_hq_mum, w4_hq_mum),
    educdtlpa_raw = coalesce(w1_hq_dad, w2_hq_dad, w4_hq_dad)
  ) %>%
  mutate(
    educdtlma = ifelse(is.na(educdtlma_raw), -3, educdtlma_raw),
    educdtlpa = ifelse(is.na(educdtlpa_raw), -3, educdtlpa_raw)
  )

# Create consolidated NVQ (5‑level) variables
merged <- merged %>%
  mutate(
    w1_nvq_mum = collapse_nvq(w1_hq_mum),
    w2_nvq_mum = collapse_nvq(w2_hq_mum),
    w4_nvq_mum = collapse_nvq(w4_hq_mum),
    w1_nvq_dad = collapse_nvq(w1_hq_dad),
    w2_nvq_dad = collapse_nvq(w2_hq_dad),
    w4_nvq_dad = collapse_nvq(w4_hq_dad)
  ) %>%
  mutate(
    educma_raw = coalesce(w1_nvq_mum, w2_nvq_mum, w4_nvq_mum),
    educpa_raw = coalesce(w1_nvq_dad, w2_nvq_dad, w4_nvq_dad)
  ) %>%
  mutate(
    educma = ifelse(is.na(educma_raw), -3, educma_raw),
    educpa = ifelse(is.na(educpa_raw), -3, educpa_raw)
  )

# Convert to integer (required for factor levels)
merged <- merged %>%
  mutate(
    educdtlma = as.integer(educdtlma),
    educdtlpa = as.integer(educdtlpa),
    educma   = as.integer(educma),
    educpa   = as.integer(educpa)
  )

# Define labels for the two construct types
# Detailed (20 categories + missing codes)

detailed_vals <- c(
  `-9` = "Refusal",
  `-8` = "Don\'t know / insufficient information",
  `-7` = "Prefer not to say",
  `-6` = "",
  `-5` = "",
  `-4` = "",
  `-3` = "Not asked at fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable",
  `1`  = "Higher Degree",
  `2`  = "First Degree",
  `3`  = "HE Diploma",
  `4`  = "HNC/HND/NVQ4",
  `5`  = "Teaching qualification, non-degree",
  `6`  = "Nursing qualification, non-degree",
  `7`  = "A Levels",
  `8`  = "OND/ONC",
  `9`  = "City and guilds part III, NVQ3",
  `10` = "CSYS",
  `11` = "Scottish Higher Grade",
  `12` = "AS Level",
  `13` = "Trade apprenticeship",
  `14` = "City and guilds part II, NVQ2",
  `15` = "GCSE grade A‑C and equivalent",
  `16` = "GCSE grade D‑E and equivalent",
  `17` = "City and guilds part I, NVQ1",
  `18` = "Youth training, skill seekers",
  `19` = "Qualification, level unspecified",
  `20` = "No qualification mentioned"
)

# NVQ (5 levels + missing codes)
nvq_vals <- c(
  `-9` = "Refusal",
  `-8` = "Don\'t know / insufficient information",
  `-7` = "Prefer not to say",
  `-6` = "",
  `-5` = "",
  `-4` = "",
  `-3` = "Not asked at fieldwork stage / not interviewed",
  `-2` = "Schedule not applicable / script error / information lost",
  `-1` = "Item not applicable",
  `1`  = "NVQ1",
  `2`  = "NVQ2",
  `3`  = "NVQ3",
  `4`  = "NVQ4",
  `5`  = "Other"
)

# Convert to labelled factors (maintaining factor levels)
merged <- merged %>%
  mutate(
    educdtlma = factor(
      educdtlma,
      levels = as.integer(names(detailed_vals)),
      labels = detailed_vals
    ),
    educdtlpa = factor(
      educdtlpa,
      levels = as.integer(names(detailed_vals)),
      labels = detailed_vals
    ),
    educma = factor(
      educma,
      levels = as.integer(names(nvq_vals)),
      labels = nvq_vals
    ),
    educpa = factor(
      educpa,
      levels = as.integer(names(nvq_vals)),
      labels = nvq_vals
    )
  )

# Keep only the required final variables
final_df <- merged %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Write the cleaned data
write_csv(final_df, "data/output/cleaned_data.csv")
