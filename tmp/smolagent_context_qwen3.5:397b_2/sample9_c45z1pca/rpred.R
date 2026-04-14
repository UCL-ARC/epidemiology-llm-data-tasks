library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load the three wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Select only NSID and parental education variables from each wave
wave1_sub <- wave1 %>% select(NSID, W1hiqualmum, W1hiqualdad)
wave2_sub <- wave2 %>% select(NSID, W2hiqualmum, W2hiqualdad)
wave4_sub <- wave4 %>% select(NSID, w4hiqualmum, w4hiqualdad)

# Merge using full_join by NSID
merged <- wave1_sub %>%
  full_join(wave2_sub, by = "NSID") %>%
  full_join(wave4_sub, by = "NSID")

# Function to harmonize missing codes based on value labels
harmonize_missing <- function(x, wave) {
  if (wave %in% c(1, 2)) {
    x <- case_when(
      x == -999 ~ -2,
      x == -99 ~ -3,
      x == -98 ~ -3,
      x == -94 ~ -8,
      x == -92 ~ -9,
      x == -91 ~ -1,
      x == -1 ~ -8,
      TRUE ~ as.numeric(x)
    )
  } else if (wave == 4) {
    x <- case_when(
      x == -99 ~ -3,
      x == -98 ~ -3,
      x == -94 ~ -8,
      TRUE ~ as.numeric(x)
    )
  }
  return(x)
}

# Function to consolidate across waves with priority: wave1, wave2, wave4
consolidate_var <- function(w1, w2, w4) {
  result <- rep(NA_real_, length(w1))
  
  for (i in 1:length(w1)) {
    vals <- c(w1[i], w2[i], w4[i])
    positive_vals <- vals[!is.na(vals) & vals >= 1 & vals <= 20]
    
    if (length(positive_vals) > 0) {
      if (!is.na(w1[i]) && w1[i] >= 1 && w1[i] <= 20) {
        result[i] <- w1[i]
      } else if (!is.na(w2[i]) && w2[i] >= 1 && w2[i] <= 20) {
        result[i] <- w2[i]
      } else if (!is.na(w4[i]) && w4[i] >= 1 && w4[i] <= 20) {
        result[i] <- w4[i]
      }
    } else {
      if (!is.na(w1[i]) && w1[i] < 0) {
        result[i] <- w1[i]
      } else if (!is.na(w2[i]) && w2[i] < 0) {
        result[i] <- w2[i]
      } else if (!is.na(w4[i]) && w4[i] < 0) {
        result[i] <- w4[i]
      } else if (is.na(w1[i]) && !is.na(w2[i])) {
        result[i] <- w2[i]
      } else if (is.na(w1[i]) && is.na(w2[i]) && !is.na(w4[i])) {
        result[i] <- w4[i]
      } else {
        result[i] <- -3
      }
    }
  }
  return(result)
}

# Apply harmonization to all variables
merged <- merged %>%
  mutate(
    W1hiqualmum_h = harmonize_missing(W1hiqualmum, 1),
    W1hiqualdad_h = harmonize_missing(W1hiqualdad, 1),
    W2hiqualmum_h = harmonize_missing(W2hiqualmum, 2),
    W2hiqualdad_h = harmonize_missing(W2hiqualdad, 2),
    w4hiqualmum_h = harmonize_missing(w4hiqualmum, 4),
    w4hiqualdad_h = harmonize_missing(w4hiqualdad, 4)
  )

# Consolidate maternal and paternal education
merged <- merged %>%
  mutate(
    educdtlma_raw = consolidate_var(W1hiqualmum_h, W2hiqualmum_h, w4hiqualmum_h),
    educdtlpa_raw = consolidate_var(W1hiqualdad_h, W2hiqualdad_h, w4hiqualdad_h)
  )

# Function to create collapsed variable (5-level NVQ scheme)
create_collapsed <- function(x) {
  result <- case_when(
    x >= 1 & x <= 4 ~ 0,
    x >= 5 & x <= 17 ~ 1,
    x == 18 ~ 2,
    x == 19 ~ 3,
    x == 20 ~ 4,
    x < 0 ~ as.numeric(x),
    TRUE ~ as.numeric(x)
  )
  return(result)
}

# Create collapsed variables
merged <- merged %>%
  mutate(
    educma_raw = create_collapsed(educdtlma_raw),
    educpa_raw = create_collapsed(educdtlpa_raw)
  )

# Create labelled factors using labelled() from haven
# Need to use numeric values as names properly
detailed_labs <- c(
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
  "No qualification mentioned" = 20,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked at the fieldwork stage" = -3,
  "Schedule not applicable" = -2,
  "Item not applicable" = -1
)

collapsed_labs <- c(
  "NVQ 4-5: degree-level qualifications and above" = 0,
  "NVQ 1-3: sub-degree qualifications" = 1,
  "None/entry: training programmes below NVQ level" = 2,
  "Other: qualification level unspecified" = 3,
  "No qualifications mentioned" = 4,
  "Refusal" = -9,
  "Don't know/insufficient information" = -8,
  "Not asked at the fieldwork stage" = -3,
  "Schedule not applicable" = -2,
  "Item not applicable" = -1
)

# Apply labels using labelled() and convert to factor
merged <- merged %>%
  mutate(
    educdtlma = as_factor(labelled(educdtlma_raw, labels = detailed_labs)),
    educdtlpa = as_factor(labelled(educdtlpa_raw, labels = detailed_labs)),
    educma = as_factor(labelled(educma_raw, labels = collapsed_labs)),
    educpa = as_factor(labelled(educpa_raw, labels = collapsed_labs))
  )

# Select final 5 variables
final_data <- merged %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Script completed successfully.\n")
cat("Number of rows:", nrow(final_data), "\n")
cat("Variables:", paste(names(final_data), collapse = ", "), "\n")