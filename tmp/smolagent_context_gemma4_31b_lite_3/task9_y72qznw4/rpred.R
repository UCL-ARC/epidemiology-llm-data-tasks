library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c('wave_one_lsype_family_background_2020.tab', 
           'wave_two_lsype_family_background_2020.tab', 
           'wave_four_lsype_family_background_2020.tab')

load_data <- function(filename) {
  read_delim(paste0('data/input/', filename), delim = '\t', col_types = cols(.default = 'c'))
}

df1 <- load_data(files[1])
df2 <- load_data(files[2])
df4 <- load_data(files[3])

# Merge datasets
full_df <- df1 %>% 
  full_join(df2, by = 'NSID') %>% 
  full_join(df4, by = 'NSID')

# Convert source variables to numeric
full_df <- full_df %>%
  mutate(
    W1hiqualmum = as.numeric(W1hiqualmum), W1hiqualdad = as.numeric(W1hiqualdad),
    W2hiqualmum = as.numeric(W2hiqualmum), W2hiqualdad = as.numeric(W2hiqualdad),
    w4hiqualmum = as.numeric(w4hiqualmum), w4hiqualdad = as.numeric(w4hiqualdad)
  )

# 2. Missing Value Harmonisation Function
harmonise_missing <- function(x, labels_map) {
  # Map based on label meanings provided in metadata
  # -999: household data lost -> -2
  # -99: not interviewed -> -3 (Standard NA/Not asked)
  # -98: not present -> -3 (Standard NA)
  # -94: insufficient info -> -8
  # -92: refused -> -9
  # -91: not applicable -> -1
  # -1: don't know -> -8
  
  res <- x
  res[x == -999] <- -2
  res[x == -99]  <- -3
  res[x == -98]  <- -3
  res[x == -94]  <- -8
  res[x == -92]  <- -9
  res[x == -91]  <- -1
  res[x == -1]   <- -8
  res[is.na(x)]  <- -3
  return(res)
}

# Apply missing value harmonisation
full_df <- full_df %>%
  mutate(
    W1hiqualmum_m = harmonise_missing(W1hiqualmum),
    W1hiqualdad_m = harmonise_missing(W1hiqualdad),
    W2hiqualmum_m = harmonise_missing(W2hiqualmum),
    W2hiqualdad_m = harmonise_missing(W2hiqualdad),
    w4hiqualmum_m = harmonise_missing(w4hiqualmum),
    w4hiqualdad_m = harmonise_missing(w4hiqualdad)
  )

# 3. Consolidation (Earliest-Valid-First)
consolidate <- function(v1, v2, v3) {
  # Valid substantive responses are >= 1
  res <- v1
  valid_v1 <- !is.na(v1) & v1 >= 1
  
  # If v1 not valid, try v2
  res[!valid_v1] <- coalesce(v2[!valid_v1], v3[!valid_v1])
  
  # Final pass: if still NA or not valid, we keep the missing code from the latest wave or -3
  # But the rule is earliest-valid-first. If no valid found, use the most descriptive missing code available.
  # Let's refine: prioritize values >= 1, then missing codes.
  
  # Correct Earliest-Valid-First implementation
  out <- v1
  # If v1 is missing, take v2
  out[v1 < 1] <- v2[v1 < 1]
  # If still missing, take v3
  out[out < 1] <- v3[out < 1]
  
  # Handle NAs (convert to -3)
  out[is.na(out)] <- -3
  return(out)
}

full_df <- full_df %>%
  mutate(
    educdtlma = consolidate(W1hiqualmum_m, W2hiqualmum_m, w4hiqualmum_m),
    educdtlpa = consolidate(W1hiqualdad_m, W2hiqualdad_m, w4hiqualdad_m)
  )

# 4. NVQ Harmonisation (5-level NVQ)
# 1: Higher Degree, 2: First Degree, 3: HE Diploma
# 4: HNC/HND/NVQ4
# 5: A Levels, OND/ONC, NVQ3, etc (Level 3)
# 6: NVQ2, GCSE A-C, etc (Level 2)
# 7: NVQ1, GCSE D-E, etc (Level 1)
# 8: No qualification

# Requirements: 5-level NVQ. 
# Typical mapping:
# 1. Degree or higher (1, 2, 3)
# 2. NVQ 4 / HNC / HND (4)
# 3. NVQ 3 / A Level / Scottish Higher (7, 8, 9, 10, 11, 12)
# 4. NVQ 2 / GCSE A-C (14, 15)
# 5. NVQ 1 / GCSE D-E / No qual (16, 17, 18, 19, 20)

map_nvq <- function(x) {
  res <- x
  res[x >= 1 & x <= 3] <- 1
  res[x == 4] <- 2
  res[x >= 7 & x <= 12] <- 3
  res[x == 14 | x == 15] <- 4
  res[x >= 16 & x <= 20] <- 5
  return(res)
}

full_df <- full_df %>%
  mutate(
    educma = map_nvq(educdtlma),
    educpa = map_nvq(educdtlpa)
  )

# Clean up NVQ: if it was missing in detailed, it should be missing in NVQ
full_df$educma[full_df$educdtlma < 1] <- full_df$educdtlma[full_df$educdtlma < 1]
full_df$educpa[full_df$educdtlpa < 1] <- full_df$educdtlpa[full_df$educdtlpa < 1]

# 5. Final Dataframe
final_df <- full_df %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

# Factor labelling for educma/educpa
nvq_labels <- c(
  "1" = "Degree or higher",
  "2" = "NVQ 4 / HNC / HND",
  "3" = "NVQ 3 / A Level",
  "4" = "NVQ 2 / GCSE A-C",
  "5" = "NVQ 1 / Lower GCSE / No qualification",
  "-1" = "Not applicable",
  "-2" = "Schedule not applicable",
  "-3" = "Not asked",
  "-7" = "Prefer not to say",
  "-8" = "Don't know",
  "-9" = "Refused"
)

# Applying labels
final_df$educma <- factor(final_df$educma, levels = names(nvq_labels), labels = nvq_labels)
final_df$educpa <- factor(final_df$educpa, levels = names(nvq_labels), labels = nvq_labels)

write_csv(final_df, 'data/output/cleaned_data.csv')
