library(haven)
library(dplyr)
library(readr)
library(labelled)

# Create output directory
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all datasets
data_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
data_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
data_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
data_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
data_eight <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
data_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Recode all NA values to -3
recode_na <- function(df) {
  df %>% mutate(across(everything(), ~ ifelse(is.na(.), -3, .)))
}

data_one <- recode_na(data_one)
data_four <- recode_na(data_four)
data_six <- recode_na(data_six)
data_seven <- recode_na(data_seven)
data_eight <- recode_na(data_eight)
data_nine <- recode_na(data_nine)

# Wave 1 (Age 14): No education variable
data_one <- data_one %>% select(NSID)

# Wave 4 (Age 17): w4saim variable
data_four <- data_four %>%
  mutate(
    educaim17 = case_when(
      is.na(w4saim) | w4saim %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -991, -990, -1) ~ -3,
      w4saim == 14 ~ 5,  # Not studying
      w4saim == -91 ~ 5,  # Not studying
      TRUE ~ 3  # Default to Other
    )
  ) %>% select(NSID, educaim17)

# Wave 6 (Age 19): W6Saim variable
data_six <- data_six %>%
  mutate(
    educaim19 = case_when(
      is.na(W6Saim) | W6Saim %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -991, -990, -1) ~ -3,
      W6Saim == 16 ~ 5,  # Not studying
      W6Saim %in% c(1, 3, 13) ~ 0,  # NVQ 5, NVQ 4, NVQ 5
      W6Saim %in% c(2, 4, 12) ~ 0,  # First/Other Degree, Other HE
      W6Saim %in% c(5, 9, 10) ~ 1,  # NVQ 3, NVQ 2, Other level 2
      W6Saim %in% c(6, 7) ~ 1,  # AVCE, A/AS
      W6Saim %in% c(8, 11) ~ 1,  # Other level 3, GCSE
      W6Saim == 14 ~ 3,  # Other (level unknown)
      W6Saim == 15 ~ -3,  # No detail
      TRUE ~ 3
    )
  ) %>% select(NSID, educaim19)

# Wave 7 (Age 20): W7SAim variable
data_seven <- data_seven %>%
  mutate(
    educaim20 = case_when(
      is.na(W7SAim) | W7SAim %in% c(-999, -998, -997, -996, -995, -994, -993, -992, -991, -990, -1) ~ -3,
      W7SAim == -91 ~ 5,  # Not applicable (not studying)
      W7SAim == -94 ~ -2,  # Insufficient information
      W7SAim %in% c(10, 13) ~ 0,  # NVQ 4, NVQ 5
      W7SAim %in% c(11, 12, 14) ~ 0,  # First/Other Degree, Other HE
      W7SAim %in% c(1, 3, 6) ~ 1,  # NVQ 1, NVQ 2, NVQ 3
      W7SAim %in% c(2, 5, 8) ~ 1,  # Other level 1, Other level 2, AVCE
      W7SAim %in% c(4, 7, 9) ~ 1,  # GCSE, A/AS, Other level 3
      TRUE ~ 3
    )
  ) %>% select(NSID, educaim20)

# Wave 8 (Age 25): Complex with activity + qualifications
data_eight <- data_eight %>%
  mutate(
    # Study status
    study_status = case_when(
      W8ACTIVITY05 == 1 ~ 1,  # Studying
      W8ACTIVITY05 == 0 ~ 0,  # Not studying
      W8ACTIVITY05 %in% c(-9, -8, -1) ~ -1,
      TRUE ~ NA_real_
    ),
    # Higher level qualifiers (category 0)
    higher_qual = W8ACQUC0A == 1 | W8ACQUC0B == 1 | W8ACQUC0C == 1 |
                  W8ACQUC0D == 1 | W8ACQUC0E == 1 |
                  W8VCQUC0J == 1 | W8VCQUC0K == 1,
    # Lower level qualifiers (category 1)
    lower_qual = W8ACQUC0F == 1 | W8ACQUC0G == 1 | W8ACQUC0H == 1 | W8ACQUC0I == 1 |
                 W8ACQUC0J == 1 | W8ACQUC0K == 1 | W8ACQUC0L == 1 | W8ACQUC0M == 1 |
                 W8ACQUC0N == 1 |
                 W8VCQUC0B == 1 | W8VCQUC0C == 1 | W8VCQUC0D == 1 | W8VCQUC0E == 1,
    # None of these (category 4)
    none_qual = W8ACQUC0O == 1,
    # Don't know (category 3)
    dk_qual = W8ACQUC0P == 1,
    # Refused (category 3)
    refused_qual = W8ACQUC0Q == 1
  ) %>%
  mutate(
    educaim25 = case_when(
      W8ACTIVITY05 == 0 ~ 5,  # Not studying
      W8ACTIVITY05 == 1 & higher_qual ~ 0,  # Higher level
      W8ACTIVITY05 == 1 & lower_qual ~ 1,  # Lower level
      W8ACTIVITY05 == 1 & none_qual ~ 4,  # None of these
      W8ACTIVITY05 == 1 & dk_qual ~ 3,  # Don't know
      W8ACTIVITY05 == 1 & refused_qual ~ 3,  # Refused
      W8ACTIVITY05 == 1 ~ 3,  # Default for studying
      W8ACTIVITY05 %in% c(-9, -8, -1) ~ -3,
      TRUE ~ 3
    )
  ) %>% select(NSID, educaim25)

# Wave 9 (Age 32): Complex with economic activity + qualifications
data_nine <- data_nine %>%
  mutate(
    # Check if studying (ECONACT2 = 6 or 7)
    studying = W9ECONACT2 %in% c(6, 7),
    # Higher level qualifiers (category 0)
    higher_qual = W9ACQUC0A == 1 | W9ACQUC0B == 1 | W9ACQUC0C == 1 |
                  W9ACQUC0D == 1 | W9ACQUC0E == 1 | W9ACQUC0F == 1 |
                  W9VCQUC0A == 1 | W9VCQUC0C == 1,
    # Lower level qualifiers (category 1)
    lower_qual = W9ACQUC0G == 1 | W9ACQUC0H == 1 | W9ACQUC0I == 1 | W9ACQUC0J == 1 |
                 W9ACQUC0K == 1 | W9ACQUC0L == 1 | W9ACQUC0M == 1 | W9ACQUC0N == 1 |
                 W9ACQUC0O == 1 | W9ACQUC0P == 1 | W9ACQUC0Q == 1 | W9ACQUC0R == 1 |
                 W9VCQUC0D == 1 | W9VCQUC0E == 1 | W9VCQUC0F == 1 |
                 W9VCQUC0G == 1 | W9VCQUC0H == 1 | W9VCQUC0I == 1 |
                 W9VCQUC0O == 1 | W9VCQUC0P == 1 | W9VCQUC0Q == 1 |
                 W9VCQUC0R == 1 | W9VCQUC0S == 1 | W9VCQUC0T == 1 |
                 W9VCQUC0U == 1 | W9VCQUC0V == 1 | W9VCQUC0W == 1 | W9VCQUC0X == 1 |
                 W9VCQUC0Y == 1 | W9VCQUC0Z == 1 | W9VCQUCAA == 1 | W9VCQUCAB == 1 |
                 W9VCQUCAD == 1 | W9VCQUCAE == 1 | W9VCQUCAF == 1,
    # None of these (category 4)
    none_qual = W9ACQUC0S == 1 | W9VCQUC0G == 1,
    # Don't know (category 3)
    dk_qual = W9ACQUC0T == 1 | W9VCQUCAH == 1,
    # Refused (category 3)
    refused_qual = W9ACQUC0U == 1 | W9VCQUCAI == 1
  ) %>%
  mutate(
    educaim32 = case_when(
      studying & higher_qual ~ 0,  # Higher level
      studying & lower_qual ~ 1,  # Lower level
      studying & none_qual ~ 4,  # None of these
      studying & dk_qual ~ 3,  # Don't know
      studying & refused_qual ~ 3,  # Refused
      studying ~ 3,  # Default for studying
      W9ECONACT2 %in% c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 13, 14) ~ 5,  # Not studying
      W9ECONACT2 %in% c(-3, -1) ~ -3,  # Not asked/not applicable
      TRUE ~ 3
    )
  ) %>% select(NSID, educaim32)

# Merge all datasets
cleaned_data <- data_one %>%
  full_join(data_four, by = "NSID") %>%
  full_join(data_six, by = "NSID") %>%
  full_join(data_seven, by = "NSID") %>%
  full_join(data_eight, by = "NSID") %>%
  full_join(data_nine, by = "NSID")

# Keep only NSID and education variables
cleaned_data <- cleaned_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

print("Data cleaning complete!")
print(paste("Output written to data/output/cleaned_data.csv"))
print(paste("Rows:", nrow(cleaned_data)))