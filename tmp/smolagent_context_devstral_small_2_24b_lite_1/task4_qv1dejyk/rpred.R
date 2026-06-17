library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define metadata for each wave
wave6_metadata <- list(
  variables = list(
    W6SexualityYP = list(
      value_labels = list(
        '-97.0' = 'Respondent declined self completion',
        '-92.0' = 'Refused',
        '-91.0' = 'Not applicable',
        '-1.0' = "Don't know",
        '1.0' = 'Heterosexual / Straight',
        '2.0' = 'Gay / Lesbian',
        '3.0' = 'Bisexual',
        '4.0' = 'Other'
      )
    )
  )
)

wave7_metadata <- list(
  variables = list(
    W7SexualityYP = list(
      value_labels = list(
        '-100.0' = 'Respondent declined sexual experience questions',
        '-97.0' = 'Refused self completion',
        '-92.0' = 'Refused',
        '-91.0' = 'Not applicable',
        '-1.0' = "Don't know",
        '1.0' = 'Heterosexual / Straight',
        '2.0' = 'Gay / Lesbian',
        '3.0' = 'Bisexual',
        '4.0' = 'Other'
      )
    )
  )
)

wave8_metadata <- list(
  variables = list(
    W8SEXUALITY = list(
      value_labels = list(
        '-9.0' = 'Refused',
        '-8.0' = "Don't know",
        '-1.0' = 'Not applicable',
        '1.0' = 'Heterosexual / Straight',
        '2.0' = 'Gay / Lesbian',
        '3.0' = 'Bisexual',
        '4.0' = 'Other'
      )
    )
  )
)

wave9_metadata <- list(
  variables = list(
    W9SORI = list(
      value_labels = list(
        '-9.0' = 'Refused',
        '-8.0' = "Don't know",
        '-3.0' = 'Not asked at fieldwork stage',
        '-1.0' = 'Not applicable',
        '1.0' = 'Heterosexual / Straight',
        '2.0' = 'Gay / Lesbian',
        '3.0' = 'Bisexual',
        '4.0' = 'Other',
        '5.0' = 'Prefer not to say'
      )
    )
  )
)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to harmonize missing values
harmonize_missing <- function(var, metadata) {
  var <- as.numeric(var)
  
  # Map missing values based on metadata labels
  if (!is.null(metadata$value_labels)) {
    for (code in names(metadata$value_labels)) {
      label <- metadata$value_labels[[code]]
      
      if (label == "Refused" || label == "Refused self completion") {
        var[var == as.numeric(code)] <- -9
      } else if (label == "Don't know" || label == "Insufficient information") {
        var[var == as.numeric(code)] <- -8
      } else if (label == "Prefer not to say") {
        var[var == as.numeric(code)] <- -7
      } else if (label == "Not asked at the fieldwork stage" || label == "Not asked at fieldwork stage") {
        var[var == as.numeric(code)] <- -3
      } else if (label == "Schedule not applicable" || label == "Script error" || label == "Information lost" || label == "Not applicable") {
        var[var == as.numeric(code)] <- -2
      } else if (label == "Respondent declined self completion" || label == "Respondent declined sexual experience questions") {
        var[var == as.numeric(code)] <- -9
      }
    }
  }
  
  # Convert NA to -3
  var[is.na(var)] <- -3
  
  return(var)
}

# Harmonize sexual orientation variables
wave6$W6SexualityYP <- harmonize_missing(wave6$W6SexualityYP, wave6_metadata$variables$W6SexualityYP)
wave7$W7SexualityYP <- harmonize_missing(wave7$W7SexualityYP, wave7_metadata$variables$W7SexualityYP)
wave8$W8SEXUALITY <- harmonize_missing(wave8$W8SEXUALITY, wave8_metadata$variables$W8SEXUALITY)
wave9$W9SORI <- harmonize_missing(wave9$W9SORI, wave9_metadata$variables$W9SORI)

# Create time-varying sexual orientation variables
merged_data <- merged_data %>%
  mutate(
    sori19 = case_when(!is.na(W6SexualityYP) ~ W6SexualityYP, TRUE ~ -3),
    sori20 = case_when(!is.na(W7SexualityYP) ~ W7SexualityYP, TRUE ~ -3),
    sori25 = case_when(!is.na(W8SEXUALITY) ~ W8SEXUALITY, TRUE ~ -3),
    sori32 = case_when(!is.na(W9SORI) ~ W9SORI, TRUE ~ -3)
  )

# Select only NSID and derived variables
output_data <- merged_data %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")