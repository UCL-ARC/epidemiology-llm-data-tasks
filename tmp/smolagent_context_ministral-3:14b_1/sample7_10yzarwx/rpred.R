library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave_eight <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave_nine <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Merge all datasets by NSID
merged_data <- wave_one %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(wave_eight, by = "NSID") %>%
  full_join(wave_nine, by = "NSID")

# Function to recode missing values
recode_missing <- function(x) {
  x <- ifelse(x >= -999 & x <= -1, x, x)
  x <- ifelse(x == -999, -9, x)  # Refusal
  x <- ifelse(x == -998, -8, x)  # Don't know
  x <- ifelse(x == -997, -1, x)  # Not applicable
  x <- ifelse(x == -94, -2, x)   # Insufficient information
  x <- ifelse(x == -91, -1, x)   # Not applicable
  x <- ifelse(is.na(x), -3, x)    # Not asked
  return(x)
}

# Function to harmonize education variables for teenage waves
harmonize_teenage_education <- function(data, wave, age, aim_var) {
  # Recode missing values
  data[[aim_var]] <- recode_missing(data[[aim_var]])

  # Define mapping for the specific wave
  if (wave == "wave_four") {
    data[[paste0("educaim", age)]] <- case_when(
      data[[aim_var]] %in% c(1, 5, 9) ~ 2,  # NVQ 1, NVQ 2, NVQ 3
      data[[aim_var]] %in% c(2, 6, 8) ~ 1,  # AVCE, Intermediate GNVQ, GCSE
      data[[aim_var]] %in% c(3, 4, 7) ~ 1,  # A/AS, Other level 3, Other level 2
      data[[aim_var]] %in% c(10, 11) ~ 2,  # Foundation, Other level 1
      data[[aim_var]] == 12 ~ 3,            # Other
      data[[aim_var]] == 13 ~ 3,            # No detail
      data[[aim_var]] == 14 ~ 5,            # Not studying
      TRUE ~ -3
    )
  } else if (wave == "wave_six") {
    data[[paste0("educaim", age)]] <- case_when(
      data[[aim_var]] %in% c(1, 3) ~ 0,     # NVQ 5, NVQ 4
      data[[aim_var]] %in% c(2, 4) ~ 0,     # First/Other Degree, Other HE
      data[[aim_var]] %in% c(5, 9, 12) ~ 2,  # NVQ 3, NVQ 2, NVQ 1
      data[[aim_var]] %in% c(6, 7, 8) ~ 1,  # AVCE, A/AS, Other level 3
      data[[aim_var]] %in% c(10, 11, 13) ~ 1, # Other level 2, GCSE, Other level 1
      data[[aim_var]] == 14 ~ 3,            # Other (level unknown)
      data[[aim_var]] == 15 ~ 3,            # No detail
      data[[aim_var]] == 16 ~ 5,            # Not studying
      TRUE ~ -3
    )
  } else if (wave == "wave_seven") {
    data[[paste0("educaim", age)]] <- case_when(
      data[[aim_var]] %in% c(10, 11, 12, 13) ~ 0, # NVQ 4, First/Other Degree, Other HE, NVQ 5
      data[[aim_var]] %in% c(1, 3, 6) ~ 2,         # NVQ 1, NVQ 2, NVQ 3
      data[[aim_var]] %in% c(2, 4, 5, 7, 8, 9) ~ 1, # Other level 1, GCSE, Other level 2, A/AS, AVCE, Other level 3
      data[[aim_var]] == 14 ~ 3,                 # Other (level unknown)
      TRUE ~ -3
    )
  }

  return(data)
}

# Function to handle adult waves (Wave 8 and Wave 9)
harmonize_adult_education <- function(data, wave, age) {
  # Recode missing values for activity variables
  activity_var <- switch(
    wave,
    "wave_eight" = "W8ACTIVITY05",
    "wave_nine" = "W9ECONACT2"
  )

  data[[activity_var]] <- recode_missing(data[[activity_var]])

  # Determine if studying
  studying <- ifelse(data[[activity_var]] %in% c(6, 7), 1, 0)

  # Define qualification variables for each wave
  qual_vars <- switch(
    wave,
    "wave_eight" = c(
      "W8ACQUC0A", "W8ACQUC0B", "W8ACQUC0C", "W8ACQUC0D", "W8ACQUC0E",  # Academic qualifications
      "W8VCQUC0J", "W8VCQUC0K", "W8VCQUC0A", "W8VCQUC0B", "W8VCQUC0C", "W8VCQUC0D", "W8VCQUC0E"  # Vocational qualifications
    ), 
    "wave_nine" = c(
      "W9ACQUC0A", "W9ACQUC0B", "W9ACQUC0C", "W9ACQUC0D", "W9ACQUC0E",  # Academic qualifications
      "W9VCQUC0A", "W9VCQUC0C", "W9VCQUC0D", "W9VCQUC0E", "W9VCQUC0F", "W9VCQUC0G", "W9VCQUC0H", "W9VCQUC0I", "W9VCQUC0J", "W9VCQUC0K", "W9VCQUC0L", "W9VCQUC0M", "W9VCQUC0N", "W9VCQUC0O", "W9VCQUC0P", "W9VCQUC0Q", "W9VCQUC0R", "W9VCQUC0S", "W9VCQUC0T", "W9VCQUC0U", "W9VCQUC0V", "W9VCQUC0W", "W9VCQUC0X", "W9VCQUC0Y", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAB", "W9VCQUCAC", "W9VCQUCAD", "W9VCQUCAE", "W9VCQUCAF"  # Vocational qualifications
    )
  )

  # Recode all qualification variables
  for (var in qual_vars) {
    data[[var]] <- recode_missing(data[[var]])
  }

  # Create a new variable for education aim
  data[[paste0("educaim", age)]] <- -3  # Default to not asked

  # If not studying, assign category 5
  data$educaim_temp <- ifelse(studying == 0, 5, -3)

  # If studying, classify into categories 0-4
  if (wave == "wave_eight") {
    # NVQ 4-5 (higher/HE level)
    higher_qual <- rowSums(data %>% select(all_of(c("W8ACQUC0A", "W8ACQUC0B", "W8ACQUC0C", "W8ACQUC0D", "W8ACQUC0E", "W8VCQUC0J", "W8VCQUC0K")))) > 0

    # NVQ 1-3 (lower/mid level)
    mid_qual <- rowSums(data %>% select(all_of(c("W8VCQUC0A", "W8VCQUC0B", "W8VCQUC0C", "W8VCQUC0D", "W8VCQUC0E")))) > 0

    # None/entry level
    entry_qual <- rowSums(data %>% select(all_of(c("W8VCQUC0C", "W8VCQUC0D")))) > 0

    # Other
    other_qual <- rowSums(data %>% select(all_of(c("W8ACQUC0P", "W8ACQUC0Q", "W8VCQUC0F")))) > 0

    # None of these qualifications
    none_qual <- rowSums(data %>% select(all_of("W8ACQUC0O"))) > 0

    # Assign categories based on studying status
    data$educaim_temp <- ifelse(
      studying == 1,
      case_when(
        higher_qual ~ 0,
        mid_qual ~ 1,
        entry_qual ~ 2,
        other_qual ~ 3,
        none_qual ~ 4,
        TRUE ~ data$educaim_temp
      ),
      data$educaim_temp
    )
  }

  if (wave == "wave_nine") {
    # NVQ 4-5 (higher/HE level)
    higher_qual <- rowSums(data %>% select(all_of(c("W9ACQUC0A", "W9ACQUC0B", "W9ACQUC0C", "W9ACQUC0D", "W9ACQUC0E", "W9VCQUC0A", "W9VCQUC0C", "W9VCQUC0D", "W9VCQUC0R", "W9VCQUC0S", "W9VCQUCAC")))) > 0

    # NVQ 1-3 (lower/mid level)
    mid_qual <- rowSums(data %>% select(all_of(c("W9VCQUC0E", "W9VCQUC0F", "W9VCQUC0H", "W9VCQUC0I", "W9VCQUC0J", "W9VCQUC0K", "W9VCQUC0L", "W9VCQUC0M", "W9VCQUC0N", "W9VCQUC0P", "W9VCQUC0Q", "W9VCQUC0U", "W9VCQUC0X")))) > 0

    # None/entry level
    entry_qual <- rowSums(data %>% select(all_of(c("W9VCQUC0F", "W9VCQUC0K", "W9VCQUC0Y", "W9VCQUC0Z", "W9VCQUCAA", "W9VCQUCAB")))) > 0

    # Other
    other_qual <- rowSums(data %>% select(all_of(c("W9ACQUC0R", "W9VCQUC0F", "W9VCQUCAF")))) > 0

    # None of these qualifications
    none_qual <- rowSums(data %>% select(all_of(c("W9ACQUC0S", "W9VCQUCAG")))) > 0

    # Assign categories based on studying status
    data$educaim_temp <- ifelse(
      studying == 1,
      case_when(
        higher_qual ~ 0,
        mid_qual ~ 1,
        entry_qual ~ 2,
        other_qual ~ 3,
        none_qual ~ 4,
        TRUE ~ data$educaim_temp
      ),
      data$educaim_temp
    )
  }

  # Assign final education aim variable
  data[[paste0("educaim", age)]] <- data$educaim_temp
  data <- data %>% select(-educaim_temp)

  return(data)
}

# Harmonize education variables for teenage waves
merged_data <- harmonize_teenage_education(merged_data, "wave_four", 17, "w4saim")
merged_data <- harmonize_teenage_education(merged_data, "wave_six", 19, "W6Saim")
merged_data <- harmonize_teenage_education(merged_data, "wave_seven", 20, "W7SAim")

# Harmonize education variables for adult waves
merged_data <- harmonize_adult_education(merged_data, "wave_eight", 25)
merged_data <- harmonize_adult_education(merged_data, "wave_nine", 32)

# Select only NSID and education variables
final_data <- merged_data %>%
  select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write the final cleaned data to a CSV file
write_csv(final_data, "data/output/cleaned_data.csv")