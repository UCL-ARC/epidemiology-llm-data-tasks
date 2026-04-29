library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
path <- "data/input/"

load_tab <- function(f) read_delim(paste0(path, f), delim = "\t")

df1 <- load_tab("wave_one_lsype_young_person_2020.tab")
df4 <- load_tab("wave_four_lsype_young_person_2020.tab")
df5 <- load_tab("wave_five_lsype_young_person_2020.tab")
df6 <- load_tab("wave_six_lsype_young_person_2020.tab")
df7 <- load_tab("wave_seven_lsype_young_person_2020.tab")
df8 <- load_tab("ns8_2015_derived.tab")
df9 <- load_tab("ns9_2022_derived_variables.tab")

# Merge datasets
full_df <- df1 %>%
  full_join(df4, by = "NSID") %>%
  full_join(df5, by = "NSID") %>%
  full_join(df6, by = "NSID") %>%
  full_join(df7, by = "NSID") %>%
  full_join(df8, by = "NSID") %>%
  full_join(df9, by = "NSID")

# Vectorized Missing Codes Mapping Function
harmonise_missing_vec <- function(val, wave) {
  res <- rep(-3, length(val))
  
  # Default NA to -3 is already handled by initialization
  
  if (wave == "W4") {
    res[val == -999] <- -2
    res[val == -94]  <- -8
    res[val == -92]  <- -9
    res[val == -91]  <- -1
  } else if (wave == "W5") {
    res[val == -94]  <- -8
  } else if (wave == "W6") {
    res[val == -91]  <- -1
  } else if (wave == "W7") {
    res[val == -91]  <- -1
  } else if (wave == "W8" || wave == "W9") {
    res[val == -9]   <- -9
    res[val == -8]   <- -8
    res[val == -1]   <- -1
  }
  
  # Any other negative values not caught by specific wave logic
  neg_idx <- which(val < 0 & res == -3)
  res[neg_idx] <- -2
  
  # Preserve substantive values
  subst_idx <- which(val >= 0)
  res[subst_idx] <- val[subst_idx]
  
  # Final NA check
  res[is.na(val)] <- -3
  
  return(res)
}

# Vectorized 6-category mapping
map_to_collapsed_vec <- function(val, wave) {
  res <- rep(NA, length(val))
  
  if (wave == "W4") {
    res[val == 1 | val == 2] <- 1
    res[val == 4] <- 2
    res[val == 5] <- 3
    res[val == 3] <- 4
    res[val == 6] <- 5
    res[val == 7 | val == 8 | val == 9] <- 6
  } else if (wave == "W5") {
    res[val == 3] <- 1
    res[val == 1 | val == 2 | val == 5 | val == 6] <- 2
    res[val == 4] <- 3
    res[val == 7] <- 4
    res[val == 8] <- 5
    res[val == 9 | val == 10 | val == 11] <- 6
  } else if (wave == "W6") {
    res[val == 3] <- 1
    res[val == 4 | val == 5 | val == 10] <- 2
    res[val == 1 | val == 2] <- 3
    res[val == 8] <- 4
    res[val == 7] <- 5
    res[val == 6 | val == 9 | val == 11] <- 6
  } else if (wave == "W7") {
    res[val == 3] <- 1
    res[val == 4 | val == 5 | val == 11] <- 2
    res[val == 1 | val == 2 | val == 9] <- 3
    res[val == 8] <- 4
    res[val == 7] <- 5
    res[val == 6 | val == 10 | val == 12 | val == 13 | val == 14 | val == 15] <- 6
  } else if (wave == "W8" || wave == "W9") {
    res[val == 1 | val == 2] <- 1
    res[val == 6 | val == 7] <- 2
    res[val == 5] <- 3
    res[val == 4] <- 4
    res[val == 9] <- 5
    res[val == 3 | val == 8 | val == 10] <- 6
  }
  
  return(res)
}

# Processing
final_df <- full_df %>%
  mutate(
    # W4
    ecoact17_raw = map_to_collapsed_vec(W4empsYP, "W4"),
    ecoact17_miss = harmonise_missing_vec(W4empsYP, "W4"),
    ecoact17 = coalesce(ecoact17_raw, ecoact17_miss),
    
    # W5
    ecoact18_raw = map_to_collapsed_vec(W5mainactYP, "W5"),
    ecoact18_miss = harmonise_missing_vec(W5mainactYP, "W5"),
    ecoact18 = coalesce(ecoact18_raw, ecoact18_miss),
    
    # W6
    ecoact19_raw = map_to_collapsed_vec(W6TCurrentAct, "W6"),
    ecoact19_miss = harmonise_missing_vec(W6TCurrentAct, "W6"),
    ecoact19 = coalesce(ecoact19_raw, ecoact19_miss),
    
    # W7
    ecoact20_raw = map_to_collapsed_vec(W7TCurrentAct, "W7"),
    ecoact20_miss = harmonise_missing_vec(W7TCurrentAct, "W7"),
    ecoact20 = coalesce(ecoact20_raw, ecoact20_miss),
    
    # W8
    ecoact25_raw = map_to_collapsed_vec(W8DACTIVITYC, "W8"),
    ecoact25_miss = harmonise_missing_vec(W8DACTIVITYC, "W8"),
    ecoact25 = coalesce(ecoact25_raw, ecoact25_miss),
    ecoactadu25 = harmonise_missing_vec(W8DACTIVITYC, "W8"),
    
    # W9
    ecoact32_raw = map_to_collapsed_vec(W9DACTIVITYC, "W9"),
    ecoact32_miss = harmonise_missing_vec(W9DACTIVITYC, "W9"),
    ecoact32 = coalesce(ecoact32_raw, ecoact32_miss),
    ecoactadu32 = harmonise_missing_vec(W9DACTIVITYC, "W9")
  ) %>%
  select(NSID, ecoact17, ecoact18, ecoact19, ecoact20, ecoact25, ecoactadu25, ecoact32, ecoactadu32)

write_csv(final_df, "data/output/cleaned_data.csv")
