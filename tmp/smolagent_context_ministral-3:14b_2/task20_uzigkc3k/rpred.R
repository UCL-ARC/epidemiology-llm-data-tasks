
# Load required libraries
library(haven)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

# Define file paths and corresponding wave numbers and ages
wave_info <- list(
  list(file = "data/input/wave_one_lsype_young_person_2020.tab", wave_num = 1, age = 14),
  list(file = "data/input/wave_two_lsype_young_person_2020.tab", wave_num = 2, age = 15),
  list(file = "data/input/wave_three_lsype_young_person_2020.tab", wave_num = 3, age = 16),
  list(file = "data/input/wave_four_lsype_young_person_2020.tab", wave_num = 4, age = 17),
  list(file = "data/input/wave_six_lsype_young_person_2020.tab", wave_num = 6, age = 19),
  list(file = "data/input/wave_seven_lsype_young_person_2020.tab", wave_num = 7, age = 20),
  list(file = "data/input/ns8_2015_self_completion.tab", wave_num = 8, age = 25),
  list(file = "data/input/ns9_2022_main_interview.tab", wave_num = 9, age = 32)
)

# Load and process each wave
load_wave <- function(file_path, wave_num, age) {
  data <- read_delim(file_path, delim = "\t")

  # Select and rename variables
  if (wave_num == 1) {
    data <- data %>%
      select(NSID, W1alceverYP, W1alcmonYP) %>%
      rename(alc_ever = W1alceverYP, alc_mon = W1alcmonYP)
  } else {
    data <- data %>%
      select(NSID) %>%
      mutate(alc_ever = if (wave_num == 2) as.numeric(as.character(W2alceverYP)) else
               if (wave_num == 3) as.numeric(as.character(W3alceverYP)) else
               if (wave_num == 4) as.numeric(as.character(W4AlcEverYP)) else
               if (wave_num == 6) as.numeric(as.character(W6AlcEverYP)) else
               if (wave_num == 7) as.numeric(as.character(W7AlcEverYP)) else
               if (wave_num == 8) as.numeric(as.character(W8AUDIT1)) else
               as.numeric(as.character(W9AUDIT1)))
    colnames(data)[2] <- "alc_ever"
  }

  # Convert to numeric and add age column
  data <- data %>%
    mutate(across(contains("alc_"), ~ as.numeric(as.character(.)))) %>%
    mutate(age = age)

  return(data)
}

# Load all waves
wave_data <- map(wave_info, ~ {
  tryCatch({
    load_wave(.x$file, .x$wave_num, .x$age)
  }, error = function(e) {
    message(paste("Error loading", .x$file, ":", e$message))
    return(NULL)
  })
})

# Filter out NULL entries and combine all waves
wave_data <- compact(wave_data)
combined_data <- bind_rows(wave_data)

# Create a function to determine drinking status
determine_drinking <- function(df) {
  df %>%
    mutate(is_drinker = case_when(
      age == 14 ~ if_else(!is.na(alc_ever) & !is.na(alc_mon) & alc_ever == 1 & alc_mon == 1, TRUE, FALSE),
      age %in% c(25, 32) ~ if_else(!is.na(alc_ever) & alc_ever > 1, TRUE, FALSE),
      TRUE ~ if_else(!is.na(alc_ever) & alc_ever == 1, TRUE, FALSE)
    ))
}

# Apply the function
combined_data <- determine_drinking(combined_data)

# Calculate alcfst for each individual
alcfst_data <- combined_data %>%
  group_by(NSID) %>%
  summarise(
    alcfst = if (sum(is_drinker) > 0) min(age[is_drinker]) else NA_integer_,
    all_never = all(!is_drinker & !is.na(is_drinker)),
    any_missing = any(is.na(is_drinker)),
    .groups = "drop"
  ) %>%
  mutate(
    alcfst = ifelse(all_never, 99, ifelse(any_missing | is.na(alcfst), -8, alcfst))
  )

# Convert alcfst to a factor
alcfst_data$alcfst <- factor(
  alcfst_data$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")
)

# Select only NSID and alcfst for output
final_output <- alcfst_data %>% select(NSID, alcfst)

# Write output to CSV
dir.create("data/output", showWarnings = FALSE)
write_csv(final_output, "data/output/cleaned_data.csv")

# Print confirmation message
cat("Output file 'data/output/cleaned_data.csv' has been created successfully.")
