library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
files <- list(
  wave1 = "data/input/wave_one_lsype_family_background_2020.tab",
  wave2 = "data/input/wave_two_lsype_family_background_2020.tab",
  wave3 = "data/input/wave_three_lsype_family_background_2020.tab",
  wave4 = "data/input/wave_four_lsype_family_background_2020.tab",
  wave5 = "data/input/wave_five_lsype_family_background_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_main_interview.tab",
  wave9 = "data/input/ns9_2022_derived_variables.tab"
)

# Read all files with explicit column types to avoid warnings
data_list <- lapply(files, function(file) {
  read_delim(file, delim = "\t", 
              col_types = cols(
                NSID = col_character(),
                .default = col_double()
              ),
              show_col_types = FALSE)
})

# Merge all datasets
merged_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  merged_data <- full_join(merged_data, data_list[[i]], by = "NSID")
}

# Create a helper function to convert numeric to labelled factor
create_labelled_factor <- function(x, labels) {
  levels <- names(labels)
  labels <- labels[levels]
  as_factor(x, levels = levels, labels = labels)
}

# Process each wave individually
process_wave <- function(df, age, tenure_var_name, type_var_name = NULL, owned_var_name = NULL, rented_var_name = NULL) {
  if (!is.null(type_var_name)) {
    # Handle waves with split tenure variables
    df <- df %>%
      mutate(
        hownteen = case_when(
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 1 & !is.na(`{{owned_var_name}}`) ~ `{{owned_var_name}}`,
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 1 & is.na(`{{owned_var_name}}`) ~ -3,
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 2 & !is.na(`{{rented_var_name}}`) ~ `{{rented_var_name}}`,
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 2 & is.na(`{{rented_var_name}}`) ~ -3,
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 3 ~ 8,
          TRUE ~ -3
        ),
        hown = case_when(
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 1 & !is.na(`{{owned_var_name}}`) ~ case_when(
            `{{owned_var_name}}` == 1 ~ 1,
            `{{owned_var_name}}` == 2 ~ 2,
            `{{owned_var_name}}` == 3 ~ 3,
            TRUE ~ 6
          ),
          !is.na(`{{type_var_name}}`) & `{{type_var_name}}` == 2 & !is.na(`{{rented_var_name}}`) ~ case_when(
            `{{rented_var_name}}` == 1 ~ 4,
            `{{rented_var_name}}` == 2 ~ 4,
            `{{rented_var_name}}` == 3 ~ 4,
            `{{rented_var_name}}` == 4 ~ 5,
            TRUE ~ 6
          ),
          TRUE ~ -3
        )
      )
  } else {
    # Handle waves with single tenure variable
    df <- df %>%
      mutate(
        hownteen = `{{tenure_var_name}}`,
        hown = case_when(
          `{{tenure_var_name}}` == 1 ~ 1,
          `{{tenure_var_name}}` == 2 ~ 2,
          `{{tenure_var_name}}` == 3 ~ 3,
          `{{tenure_var_name}}` == 4 ~ 4,
          `{{tenure_var_name}}` == 5 ~ 4,
          `{{tenure_var_name}}` == 6 ~ 4,
          `{{tenure_var_name}}` == 7 ~ 5,
          `{{tenure_var_name}}` == 8 ~ 6,
          TRUE ~ -3
        )
      )
  }

  # Define labels
  detailed_labels <- c(
    "1" = "Owned outright",
    "2" = "Being bought on a mortgage/bank loan",
    "3" = "Shared ownership (owns & rents property)",
    "4" = "Rented from a Council or New Town",
    "5" = "Rented from a Housing Association",
    "6" = "Rented privately",
    "7" = "Rent free",
    "8" = "Some other arrangement",
    "-1" = "Item not applicable",
    "-2" = "Script error/information lost",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-8" = "Don't know/insufficient information",
    "-9" = "Refusal"
  )

  collapsed_labels <- c(
    "1" = "Owned outright",
    "2" = "Owned, buying with help of mortgage/loan",
    "3" = "Part rent, part mortgage",
    "4" = "Rent it",
    "5" = "live rent-free",
    "6" = "Other",
    "-1" = "Item not applicable",
    "-2" = "Script error/information lost",
    "-3" = "Not asked at the fieldwork stage/participated/interviewed",
    "-8" = "Don't know/insufficient information",
    "-9" = "Refusal"
  )

  # Convert to labelled factors
  df <- df %>%
    mutate(
      hownteen = create_labelled_factor(hownteen, detailed_labels),
      hown = create_labelled_factor(hown, collapsed_labels)
    )

  # Rename columns with age suffix
  df <- df %>%
    rename(
      {{paste0("hownteen", age)}} = hownteen,
      {{paste0("hown", age)}} = hown
    ) %>%
    select(-hownteen, -hown)

  return(df)
}

# Process each age wave
# Process wave 1 (age 14)
merged_data <- process_wave(merged_data, 14, "W1hous12HH")

# Process wave 2 (age 15)
merged_data <- process_wave(merged_data, 15, "W2Hous12HH")

# Process wave 3 (age 16)
merged_data <- process_wave(merged_data, 16, "W3hous12HH")

# Process wave 4 (age 17)
merged_data <- process_wave(merged_data, 17, "W4Hous12HH")

# Process wave 5 (age 18) - split variables
merged_data <- process_wave(merged_data, 18, 
                           type_var_name = "W5Hous12HH", 
                           owned_var_name = "W5Hous12BHH", 
                           rented_var_name = "W5Hous12CHH")

# Process wave 6 (age 19) - split variables
merged_data <- process_wave(merged_data, 19, 
                           type_var_name = "W6Hous12YP", 
                           owned_var_name = "W6Hous12bYP", 
                           rented_var_name = "W6Hous12cYP")

# Process wave 7 (age 20) - split variables
merged_data <- process_wave(merged_data, 20, 
                           type_var_name = "W7Hous12YP", 
                           owned_var_name = "W7Hous12bYP", 
                           rented_var_name = "W7Hous12cYP")

# Process wave 8 (age 25)
merged_data <- process_wave(merged_data, 25, "W8TENURE")

# Process wave 9 (age 32)
merged_data <- process_wave(merged_data, 32, "W9DTENURE")

# Select only required variables
required_vars <- c("NSID")
for (age in c(14:20, 25, 32)) {
  required_vars <- c(required_vars, paste0("hownteen", age), paste0("hown", age))
}

cleaned_data <- merged_data %>%
  select(all_of(required_vars))

# Write output
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)