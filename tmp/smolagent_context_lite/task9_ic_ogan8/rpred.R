library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c('wave_one_lsype_family_background_2020.tab', 
           'wave_two_lsype_family_background_2020.tab', 
           'wave_four_lsype_family_background_2020.tab')

load_data <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = readr::cols(.default = "c"))
}

df1 <- load_data('wave_one_lsype_family_background_2020.tab')
df2 <- load_data('wave_two_lsype_family_background_2020.tab')
df4 <- load_data('wave_four_lsype_family_background_2020.tab')

# Convert relevant variables to numeric
vars_to_num <- c('W1hiqualmum', 'W1hiqualdad', 'W2hiqualmum', 'W2hiqualdad', 'w4hiqualmum', 'w4hiqualdad')

# Helper to convert and clean missing values according to standard scheme
# -9: Refusal, -8: Don't know, -7: Prefer not to say, -3: Not asked/interviewed, -2: Not applicable/Lost, -1: Not applicable
clean_missing <- function(x) {
  x <- as.numeric(x)
  # Mapping based on metadata labels
  # -999.0: Missing - household data lost -> -2
  # -99.0: Not interviewed -> -3
  # -98.0: Not present -> -3
  # -94.0: Insufficient information -> -8
  # -92.0: Refused -> -9
  # -91.0: Not applicable -> -1
  # -1.0: Don't know -> -8
  
  res <- x
  res[x == -999] <- -2
  res[x == -99] <- -3
  res[x == -98] <- -3
  res[x == -94] <- -8
  res[x == -92] <- -9
  res[x == -91] <- -1
  res[x == -1] <- -8
  
  # Convert NA to -3
  res[is.na(res)] <- -3
  return(res)
}

df1 <- df1 %>% mutate(W1hiqualmum = clean_missing(W1hiqualmum), W1hiqualdad = clean_missing(W1hiqualdad))
df2 <- df2 %>% mutate(W2hiqualmum = clean_missing(W2hiqualmum), W2hiqualdad = clean_missing(W2hiqualdad))
df4 <- df4 %>% mutate(w4hiqualmum = clean_missing(w4hiqualmum), w4hiqualdad = clean_missing(w4hiqualdad))

# Merge
full_df <- df1 %>%
  full_join(df2, by = "NSID") %>%
  full_join(df4, by = "NSID")

# 2. Harmonisation logic for 5-level NVQ (educma, educpa)
# NVQ Mapping (approximation based on standard UK levels):
# 1-6: Higher/Degree -> 1
# 7-12: A-Level/Higher -> 2
# 13-14: NVQ 2-3/Trade -> 3
# 15-18: GCSE/NVQ 1 -> 4
# 19-20: Unspecified/None -> 5

map_nvq <- function(x) {
  res <- x
  res[x >= 1 & x <= 6] <- 1
  res[x >= 7 & x <= 12] <- 2
  res[x >= 13 & x <= 14] <- 3
  res[x >= 15 & x <= 18] <- 4
  res[x >= 19 & x <= 20] <- 5
  return(res)
}

# 3. Consolidation (Earliest Valid First)
consolidate_first_valid <- function(waves) {
  # waves is a data frame of columns
  # Result is a vector
  apply(waves, 1, function(row) {
    row <- as.numeric(row)
    # Valid substantive responses are >= 1
    valid_idx <- which(row >= 1)
    if (length(valid_idx) > 0) {
      return(row[valid_idx[1]])
    } else {
      # Fallback to first available missing code
      return(row[1])
    }
  })
}

# Process Mother
mum_detailed_waves <- full_df %>% select(W1hiqualmum, W2hiqualmum, w4hiqualmum)
full_df$educdtlma <- consolidate_first_valid(mum_detailed_waves)
full_df$educma <- map_nvq(full_df$educdtlma)

# Process Father
dad_detailed_waves <- full_df %>% select(W1hiqualdad, W2hiqualdad, w4hiqualdad)
full_df$educdtlpa <- consolidate_first_valid(dad_detailed_waves)
full_df$educpa <- map_nvq(full_df$educdtlpa)

# Define labels for detailed (20 categories)
detailed_labels <- c(
  "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree", "7" = "A Levels",
  "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3", "10" = "CSYS", "11" = "Scottish Higher Grade",
  "12" = "AS Level", "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent", "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers", "19" = "Qualification, level unspecified", "20" = "No qualification mentioned",
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Not applicable/Lost", "-1" = "Not applicable"
)

# Define labels for NVQ (5 categories)
nvq_labels <- c(
  "1" = "Higher/Degree", "2" = "A-Level/Higher", "3" = "NVQ 2-3/Trade", "4" = "GCSE/NVQ 1", "5" = "None/Unspecified",
  "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Not applicable/Lost", "-1" = "Not applicable"
)

full_df$educdtlma <- factor(full_df$educdtlma, levels = names(detailed_labels), labels = detailed_labels)
full_df$educdtlpa <- factor(full_df$educdtlpa, levels = names(detailed_labels), labels = detailed_labels)
full_df$educma <- factor(full_df$educma, levels = names(nvq_labels), labels = nvq_labels)
full_df$educpa <- factor(full_df$educpa, levels = names(nvq_labels), labels = nvq_labels)

# Final selection
final_data <- full_df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

write_csv(final_data, "data/output/cleaned_data.csv")