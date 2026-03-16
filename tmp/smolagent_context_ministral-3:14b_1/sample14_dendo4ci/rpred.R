suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(purrr)
})

# Load and merge data
wave_one <- readr::read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave_two <- readr::read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave_four <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave_five <- readr::read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")
wave_six <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_2015 <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
ns9_2022 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets
merged_data <- wave_one %>%
  full_join(wave_two, by = "NSID") %>%
  full_join(wave_three, by = "NSID") %>%
  full_join(wave_four, by = "NSID") %>%
  full_join(wave_five, by = "NSID") %>%
  full_join(wave_six, by = "NSID") %>%
  full_join(wave_seven, by = "NSID") %>%
  full_join(ns8_2015, by = "NSID") %>%
  full_join(ns9_2022, by = "NSID")

# Standardize missing values
standardize_missing <- function(x) {
  x %>%
    mutate(across(where(is.numeric), ~ case_when(
      . == -999 | is.na(.) ~ -3,
      . == -998 | . == -997 | . == -995 | . == -99 | . == -92 | . == -9 | . == -2 ~ -9,
      . == -91 | . == -1 | . == -8 ~ .,
      TRUE ~ .
    )))
}

# Create tenure variables with safety checks
create_tenure_vars <- function(df) {
  # Create variables for waves 1-4 with safety checks
  df <- df %>%
    mutate(
      hown14 = ifelse(is.numeric(W1hous12HH) & "W1hous12HH" %in% names(df), W1hous12HH, -3),
      hown15 = ifelse(is.numeric(W2Hous12HH) & "W2Hous12HH" %in% names(df), W2Hous12HH, -3),
      hown16 = ifelse(is.numeric(W3hous12HH) & "W3hous12HH" %in% names(df), W3hous12HH, -3),
      hown17 = ifelse(is.numeric(W4Hous12HH) & "W4Hous12HH" %in% names(df), W4Hous12HH, -3),
      hown25 = ifelse(is.numeric(W8TENURE) & "W8TENURE" %in% names(df), 
        case_when(
          W8TENURE %in% c(1, 2, 3) ~ 1,
          W8TENURE %in% c(4, 5, 6, 7) ~ 2,
          W8TENURE %in% c(-9, -8, -1, -3, -2) ~ W8TENURE,
          TRUE ~ -3
        ), -3),
      hown32 = ifelse(is.numeric(W9DTENURE) & "W9DTENURE" %in% names(df), 
        case_when(
          W9DTENURE %in% c(1, 2, 3) ~ 1,
          W9DTENURE %in% c(4, 5, 6, 7) ~ 2,
          W9DTENURE %in% c(-9, -8, -1, -3, -2) ~ W9DTENURE,
          TRUE ~ -3
        ), -3)
    )

  # Create combined 14-20 variable
  df <- df %>% mutate(
    hown14_20 = case_when(
      hown14 != -3 ~ hown14,
      hown15 != -3 ~ hown15,
      hown16 != -3 ~ hown16,
      hown17 != -3 ~ hown17,
      TRUE ~ -3
    )
  )

  return(df)
}

# Process data
cleaned_data <- merged_data %>%
  standardize_missing() %>%
  create_tenure_vars()

# Ensure all required variables exist
required_vars <- c("NSID", "hown14_20", "hown25", "hown32")
for(var in required_vars) {
  if(!var %in% names(cleaned_data)) {
    cleaned_data[[var]] <- -3
  }
}

# Add factor levels
cleaned_data <- cleaned_data %>% 
  mutate(
    hown14_20 = factor(hown14_20, 
      levels = c(1, 2, -1, -2, -3, -8, -9), 
      labels = c("Owned", "Rented", "Item not applicable", 
                 "Script error", "Not asked", "Don't know", "Refusal")),
    hown25 = factor(hown25, 
      levels = c(1, 2, -1, -2, -3, -8, -9), 
      labels = c("Owned", "Rented", "Item not applicable", 
                 "Script error", "Not asked", "Don't know", "Refusal")),
    hown32 = factor(hown32, 
      levels = c(1, 2, -1, -2, -3, -8, -9), 
      labels = c("Owned", "Rented", "Item not applicable", 
                 "Script error", "Not asked", "Don't know", "Refusal"))
  )

# Select final variables
final_vars <- c("NSID", "hown14_20", "hown25", "hown32")
cleaned_data <- cleaned_data %>% select(all_of(final_vars))

# Write output with explicit path
output_path <- "data/output/cleaned_data.csv"
if(!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}
write.csv(cleaned_data, output_path, row.names = FALSE)

# Verify output was created
file.exists(output_path)