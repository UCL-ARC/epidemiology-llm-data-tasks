library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
files <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'wave_six_lsype_young_person_2020.tab',
  'wave_seven_lsype_young_person_2020.tab',
  'ns8_2015_self_completion.tab',
  'ns9_2022_main_interview.tab'
)

data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t'))
names(data_list) <- files

# Merge datasets
full_df <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_df <- full_join(full_df, data_list[[i]], by = 'NSID')
}

# 3. Standard Missing-Value Codes mapping function
# -9 = Refusal, -8 = Don't know, -7 = Prefer not to say, -1 = Not applicable, -3 = Not asked, -2 = Schedule not applicable

clean_missing <- function(x, mapping = NULL) {
  # Map nulls to -3 first
  x[is.na(x)] <- -3
  
  if (!is.null(mapping)) {
    for (old_val in names(mapping)) {
      x[x == as.numeric(old_val)] <- mapping[[old_val]]
    }
  }
  return(x)
}

# 4 & 5. Response Category and Missing Value Harmonization for 'sori'
# Variable mapping for Sexuality/Sori across waves
# Wave 6 (Age 19): W6SexualityYP
# Wave 7 (Age 20): W7SexualityYP
# Wave 8 (Age ?): W8SEXUALITY
# Wave 9 (Age 32): W9SORI

# Mapping for W6SexualityYP
map6 <- c('-97.0' = -7, '-92.0' = -9, '-91.0' = -1, '-1.0' = -8)
sori19 <- clean_missing(full_df$W6SexualityYP, map6)

# Mapping for W7SexualityYP
map7 <- c('-100.0' = -7, '-97.0' = -7, '-92.0' = -9, '-91.0' = -1, '-1.0' = -8)
sori20 <- clean_missing(full_df$W7SexualityYP, map7)

# Mapping for W8SEXUALITY
map8 <- c('-9.0' = -9, '-8.0' = -8, '-1.0' = -1)
sori_w8 <- clean_missing(full_df$W8SEXUALITY, map8)

# Mapping for W9SORI
map9 <- c('-9.0' = -9, '-8.0' = -8, '-3.0' = -3, '-1.0' = -1, '5.0' = -7)
sori32 <- clean_missing(full_df$W9SORI, map9)

# Define factor levels for sori
sori_levels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-7" = "Prefer not to say",
  "-1" = "Item not applicable",
  "-3" = "Not asked at the fieldwork stage",
  "-2" = "Schedule not applicable",
  "1" = "Heterosexual / Straight",
  "2" = "Gay / Lesbian",
  "3" = "Bisexual",
  "4" = "Other"
)

# Convert to factors
sori19 <- factor(sori19, levels = as.numeric(names(sori_levels)), labels = sori_levels)
sori20 <- factor(sori20, levels = as.numeric(names(sori_levels)), labels = sori_levels)
sori_w8 <- factor(sori_w8, levels = as.numeric(names(sori_levels)), labels = sori_levels)
sori32 <- factor(sori32, levels = as.numeric(names(sori_levels)), labels = sori_levels)

# 6. Time-Varying vs Time-Invariant
# Sexuality is treated as time-varying here based on the request to create age-specific variables
# and the specific data provided (W6, W7, W8, W9).

# Create final output dataframe
final_df <- data.frame(NSID = full_df$NSID)
final_df$sori19 <- sori19
final_df$sori20 <- sori20
final_df$sori_w8 <- sori_w8
final_df$sori32 <- sori32

# Write to CSV
write.csv(final_df, 'data/output/cleaned_data.csv', row.names = FALSE)
