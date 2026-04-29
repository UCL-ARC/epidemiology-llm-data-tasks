library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_two_lsype_young_person_2020.tab',
  'wave_three_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_five_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_main_interview.tab',
  'ns9_2022_main_interview.tab'
)

# Load and merge
full_df <- NULL
for (f in files) {
  tmp <- readr::read_delim(paste0('data/input/', f), delim = "\t", col_types = readr::cols(.default = "c"))
  if (is.null(full_df)) {
    full_df <- tmp
  } else {
    full_df <- full_df %>% full_join(tmp, by = "NSID")
  }
}

# 2. Define Harmonisation Logic
# Standard Missing-Value Codes:
# -9 Refusal, -8 DK, -7 Prefer not to say, -3 Not asked, -2 Script error/lost, -1 Not applicable

# Process each wave's sex variable based on metadata
# W1: W1sexYP
# W2: W2SexYP
# W3: W3sexYP
# W4: W4SexYP
# W5: W5SexYP
# W6: W6Sex
# W7: W7Sex
# W8: W8CMSEX
# W9: W9DSEX

process_sex <- function(df, var_name, wave) {
  if (!var_name %in% names(df)) return(rep(-3, nrow(df)))
  
  vec <- as.numeric(df[[var_name]])
  res <- rep(-3, length(vec))
  
  # Substantive
  res[vec == 1] <- 1
  res[vec == 2] <- 2
  
  # Wave specific missing logic
  if (wave == 1) {
    res[vec == -99] <- -3
    res[vec == -92] <- -9
    res[vec == -91] <- -1
  } else if (wave == 2) {
    res[vec %in% c(-998, -997, -995)] <- -2
    res[vec == -99] <- -3
    res[vec == -92] <- -9
    res[vec == -91] <- -1
    res[vec == -1] <- -8
  } else if (wave == 3) {
    res[vec == -99] <- -3
    res[vec == -92] <- -9
    res[vec == -91] <- -1
  } else if (wave == 4) {
    res[vec == -99] <- -3
    res[vec == -92] <- -9
    res[vec == -91] <- -1
    res[vec == -1] <- -8
  } else if (wave == 5) {
    res[vec == -1] <- -8
  } else if (wave == 6) {
    res[vec == -92] <- -9
    res[vec == -91] <- -1
  } else if (wave == 7) {
    res[vec == -91] <- -1
  } else if (wave == 8) {
    res[vec == -9] <- -9
    res[vec == -8] <- -8
    res[vec == -1] <- -1
  } else if (wave == 9) {
    # No specific missing codes in metadata for W9
  }
  
  # Convert remaining NAs (from as.numeric) to -3
  res[is.na(res)] <- -3
  return(res)
}

sex_vars <- c("W1sexYP", "W2SexYP", "W3sexYP", "W4SexYP", "W5SexYP", "W6Sex", "W7Sex", "W8CMSEX", "W9DSEX")

# Create harmonised columns
for (i in 1:9) {
  full_df[[paste0("sex_w", i)]] <- process_sex(full_df, sex_vars[i], i)
}

# 3. Consolidation: Most recent response first, then fall back to oldest to newest
# This means: 
# 1. Find the most recent substantive response (W9 down to W1)
# 2. If not found, find the most recent missing code (W9 down to W1)
# 3. If still not found, use -3

sex_cols <- paste0("sex_w", 1:9)
sex_cols_rev <- rev(sex_cols)

# Find most recent substantive
full_df$sex_subst <- apply(full_df[sex_cols_rev], 1, function(x) {
  for (val in x) {
    v <- as.numeric(val)
    if (!is.na(v) && v %in% c(1, 2)) return(v)
  }
  return(NA)
})

# Find most recent any response
full_df$sex_any <- apply(full_df[sex_cols_rev], 1, function(x) {
  for (val in x) {
    v <- as.numeric(val)
    if (!is.na(v)) return(v)
  }
  return(-3)
})

full_df$sex <- ifelse(!is.na(full_df$sex_subst), full_df$sex_subst, full_df$sex_any)

# 4. Final Labels and Output
full_df$sex <- factor(full_df$sex, 
                      levels = c(1, 2, -9, -8, -7, -3, -2, -1),
                      labels = c("Male", "Female", "Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable"))

final_data <- full_df %>% select(NSID, sex)

write_csv(final_data, "data/output/cleaned_data.csv")