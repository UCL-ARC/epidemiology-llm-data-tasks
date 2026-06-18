library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Removing 'guess' shortcut as it caused an error; using default read_delim behavior
file1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
file2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
file4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge datasets
df <- file1 %>%
  full_join(file2, by = "NSID") %>%
  full_join(file4, by = "NSID")

# 2. Harmonise Missing Codes
# Detailed Labels for factors
detail_labels <- c(
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree", "7" = "A Levels", "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3", "10" = "CSYS", "11" = "Scottish Higher Grade", "12" = "AS Level",
  "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2", "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1", "18" = "Youth training, skill seekers", "19" = "Qualification, level unspecified", "20" = "No qualification mentioned"
)

# Missing labels
missing_labels <- c("-9" = "Refusal", "-8" = "Don't know / insufficient information", "-7" = "Prefer not to say", "-3" = "Not asked at the fieldwork stage / not interviewed", "-2" = "Schedule not applicable / script error / information lost", "-1" = "Item not applicable")

# Mapping dictionaries based on metadata labels
map_m1 <- c("-999" = -2, "-99" = -3, "-98" = -3, "-94" = -8, "-92" = -9, "-91" = -1)
map_m2 <- c("-999" = -2, "-99" = -3, "-98" = -3, "-94" = -8, "-92" = -9, "-91" = -1)
map_m4 <- c("-99" = -3, "-98" = -3, "-94" = -8)

map_d1 <- c("-999" = -2, "-99" = -3, "-98" = -3, "-94" = -8, "-92" = -9, "-91" = -1, "-1" = -8)
map_d2 <- c("-999" = -2, "-99" = -3, "-98" = -3, "-94" = -8, "-92" = -9, "-91" = -1, "-1" = -8)
map_d4 <- c("-99" = -3, "-98" = -3, "-94" = -8)

apply_map <- function(x, m) {
  # Convert to character to match keys in mapping dictionary
  char_x <- as.character(x)
  res <- as.numeric(char_x)
  
  # Match keys in the dictionary
  idx <- char_x %in% names(m)
  res[idx] <- m[char_x[idx]]
  
  # Handle NA values as -3
  res[is.na(res)] <- -3
  return(res)
}

df <- df %>%
  mutate(
    m1_h = apply_map(W1hiqualmum, map_m1),
    m2_h = apply_map(W2hiqualmum, map_m2),
    m4_h = apply_map(w4hiqualmum, map_m4),
    d1_h = apply_map(W1hiqualdad, map_d1),
    d2_h = apply_map(W2hiqualdad, map_d2),
    d4_h = apply_map(w4hiqualdad, map_d4)
  )

# Consolidation logic: first positive (1-20), then first negative
consolidate <- function(v1, v2, v3) {
  res <- numeric(length(v1))
  for (i in seq_along(v1)) {
    vals <- c(v1[i], v2[i], v3[i])
    pos <- vals[vals > 0][1]
    if (!is.na(pos) && !is.nan(pos)) {
      res[i] <- pos
    } else {
      neg <- vals[vals < 0][1]
      if (!is.na(neg) && !is.nan(neg)) {
        res[i] <- neg
      } else {
        res[i] <- -3
      }
    }
  }
  return(res)
}

df <- df %>%
  mutate(
    educdtlma = consolidate(m1_h, m2_h, m4_h),
    educdtlpa = consolidate(d1_h, d2_h, d4_h)
  )

# Collapsed NVQ scheme
collapse_nvq <- function(x) {
  res <- x
  res[x >= 1 & x <= 4] <- 0
  res[x >= 5 & x <= 17] <- 1
  res[x == 18] <- 2
  res[x == 19] <- 3
  res[x == 20] <- 4
  return(res)
}

df <- df %>%
  mutate(
    educma = collapse_nvq(educdtlma),
    educpa = collapse_nvq(educdtlpa)
  )

# Final Labeling
full_labels <- c(detail_labels, missing_labels)

df$educdtlma <- factor(df$educdtlma, levels = as.numeric(names(full_labels)), labels = full_labels)
df$educdtlpa <- factor(df$educdtlpa, levels = as.numeric(names(full_labels)), labels = full_labels)

nvq_labels <- c("0" = "NVQ 4–5", "1" = "NVQ 1–3", "2" = "Youth training", "3" = "Qualification, level unspecified", "4" = "No qualification mentioned")
full_nvq_labels <- c(nvq_labels, missing_labels)

df$educma <- factor(df$educma, levels = as.numeric(names(full_nvq_labels)), labels = full_nvq_labels)
df$educpa <- factor(df$educpa, levels = as.numeric(names(full_nvq_labels)), labels = full_nvq_labels)

# Output
final_df <- df %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

write_csv(final_df, "data/output/cleaned_data.csv")
