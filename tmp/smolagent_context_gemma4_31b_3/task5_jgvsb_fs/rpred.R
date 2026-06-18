library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load files
file1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", col_types = readr::cols())
file8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", col_types = readr::cols())
file9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", col_types = readr::cols())

# Merge datasets
full_df <- file1 %>%
  full_join(file4, by = "NSID") %>%
  full_join(file6, by = "NSID") %>%
  full_join(file8, by = "NSID") %>%
  full_join(file9, by = "NSID")

# Harmonisation helper for missing values
map_missing <- function(val, mapping) {
  if (is.na(val)) return(-3)
  if (val %in% names(mapping)) return(mapping[[as.character(val)]])
  return(val)
}

# W6MarStatYP mapping
# -997.0: Script error -> -2
# -97.0: Respondent declined -> -7
# -92.0: Refused -> -9
# -91.0: Not applicable -> -1
# -1.0: Don't know -> -8
map_w6 <- c("-997" = -2, "-97" = -7, "-92" = -9, "-91" = -1, "-1" = -8)

# Adult mapping (W8DMARSTAT and W9DMARSTAT)
# -9.0: Refused -> -9
# -8.0: Insufficient info -> -8
# -1.0: Not applicable -> -1
map_adult <- c("-9" = -9, "-8" = -8, "-1" = -1)

# Processing partnr19
full_df <- full_df %>%
  mutate(partnr19 = case_when(
    is.na(W6MarStatYP) ~ -3,
    W6MarStatYP == -997 ~ -2,
    W6MarStatYP == -97 ~ -7,
    W6MarStatYP == -92 ~ -9,
    W6MarStatYP == -91 ~ -1,
    W6MarStatYP == -1 ~ -8,
    TRUE ~ W6MarStatYP
  ))

# Processing partnradu25
full_df <- full_df %>%
  mutate(partnradu25 = case_when(
    is.na(W8DMARSTAT) ~ -3,
    W8DMARSTAT == -9 ~ -9,
    W8DMARSTAT == -8 ~ -8,
    W8DMARSTAT == -1 ~ -1,
    TRUE ~ W8DMARSTAT
  ))

# Processing partnradu32
full_df <- full_df %>%
  mutate(partnradu32 = case_when(
    is.na(W9DMARSTAT) ~ -3,
    W9DMARSTAT == -9 ~ -9,
    W9DMARSTAT == -8 ~ -8,
    TRUE ~ W9DMARSTAT
  ))

# Harmonisation logic for collapsed adult variables (partnr25, partnr32)
# Goal: Comparable to partnr19 (1:Single, 2:Married, 3:Separated, 4:Divorced, 5:Widowed)
# W8DMARSTAT: 1:Single/CP, 2:Married, 3:Sep(Married), 4:Div, 5:Wid, 6:CP, 7:Sep(CP), 8:FormerCP, 9:SurvivingCP
# W9DMARSTAT: 1:Single/CP, 2:Married, 3:Div, 4:Sep, 5:Wid, 6:CP, 7:FormerCP, 8:SurvivingCP

# Mapping for W8 -> partnr25
# 1, 6 -> 1 (Single/CP)
# 2 -> 2 (Married)
# 3, 7 -> 3 (Separated)
# 4, 8 -> 4 (Divorced)
# 5, 9 -> 5 (Widowed)
full_df <- full_df %>%
  mutate(partnr25 = case_when(
    partnradu25 == 1 | partnradu25 == 6 ~ 1,
    partnradu25 == 2 ~ 2,
    partnradu25 == 3 | partnradu25 == 7 ~ 3,
    partnradu25 == 4 | partnradu25 == 8 ~ 4,
    partnradu25 == 5 | partnradu25 == 9 ~ 5,
    partnradu25 < 0 ~ partnradu25,
    TRUE ~ -3
  ))

# Mapping for W9 -> partnr32
# 1, 6 -> 1 (Single/CP)
# 2 -> 2 (Married)
# 4 -> 3 (Separated)
# 3, 7 -> 4 (Divorced)
# 5, 8 -> 5 (Widowed)
full_df <- full_df %>%
  mutate(partnr32 = case_when(
    partnradu32 == 1 | partnradu32 == 6 ~ 1,
    partnradu32 == 2 ~ 2,
    partnradu32 == 4 ~ 3,
    partnradu32 == 3 | partnradu32 == 7 ~ 4,
    partnradu32 == 5 | partnradu32 == 8 ~ 5,
    partnradu32 < 0 ~ partnradu32,
    TRUE ~ -3
  ))

# Factor Labels
labels_collapsed <- c("1" = "Single", "2" = "Married", "3" = "Separated", "4" = "Divorced", "5" = "Widowed", 
                      "-9" = "Refusal", "-8" = "Don't know", "-7" = "Prefer not to say", "-3" = "Not asked", "-2" = "Not applicable", "-1" = "Not applicable")

labels_adult_w8 <- c("1" = "Single and never married or in a CP", "2" = "Married", "3" = "Separated but still legally married", "4" = "Divorced", "5" = "Widowed", "6" = "A Civil Partner", "7" = "Separated but still legally in a CP", "8" = "A former Civil Partner", "9" = "A surviving Civil Partner",
                      "-9" = "Refusal", "-8" = "Insufficient information", "-1" = "Not applicable", "-3" = "Not asked")

labels_adult_w9 <- c("1" = "Single that is never married or never in a Civil Partnership", "2" = "Married", "3" = "Divorced", "4" = "Legally separated", "5" = "Widowed", "6" = "A Civil Partner in a legally recognised Civil Partnership", "7" = "A former Civil Partner (where Civil Partnership legally dissolved)", "8" = "A surviving Civil Partner (where Civil Partner has died)",
                      "-9" = "Refusal", "-8" = "Insufficient information", "-3" = "Not asked")

full_df <- full_df %>%
  mutate(
    partnr19 = factor(partnr19, levels = as.numeric(names(labels_collapsed)), labels = labels_collapsed),
    partnr25 = factor(partnr25, levels = as.numeric(names(labels_collapsed)), labels = labels_collapsed),
    partnr32 = factor(partnr32, levels = as.numeric(names(labels_collapsed)), labels = labels_collapsed),
    partnradu25 = factor(partnradu25, levels = as.numeric(names(labels_adult_w8)), labels = labels_adult_w8),
    partnradu32 = factor(partnradu32, levels = as.numeric(names(labels_adult_w9)), labels = labels_adult_w9)
  )

# Final Selection
final_data <- full_df %>%
  select(NSID, partnr19, partnr25, partnr32, partnradu25, partnradu32)

write_csv(final_data, "data/output/cleaned_data.csv")