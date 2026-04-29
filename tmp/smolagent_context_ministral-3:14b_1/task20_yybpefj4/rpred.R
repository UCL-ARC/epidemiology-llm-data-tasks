
# Load required libraries
library(haven)
library(dplyr)
library(readr)

# Define file paths and corresponding ages
file_paths <- c(
  wave_one = "data/input/wave_one_lsype_young_person_2020.tab",
  wave_two = "data/input/wave_two_lsype_young_person_2020.tab",
  wave_three = "data/input/wave_three_lsype_young_person_2020.tab",
  wave_four = "data/input/wave_four_lsype_young_person_2020.tab",
  wave_six = "data/input/wave_six_lsype_young_person_2020.tab",
  wave_seven = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave_eight = "data/input/ns8_2015_self_completion.tab",
  wave_nine = "data/input/ns9_2022_main_interview.tab"
)

ages <- c(
  wave_one = 14,
  wave_two = 15,
  wave_three = 16,
  wave_four = 17,
  wave_six = 19,
  wave_seven = 20,
  wave_eight = 25,
  wave_nine = 32
)

# Load and process each file
load_and_process <- function(file_path, age) {
  data <- readr::read_delim(file_path, delim = "\t")

  if (age == 14) {
    data <- data %>%
      select(NSID, W1alceverYP, W1alcmonYP) %>%
      rename(alcever = W1alceverYP, alcmon = W1alcmonYP)
  } else if (age == 15) {
    data <- data %>%
      select(NSID, W2alceverYP) %>%
      rename(alcever = W2alceverYP)
  } else if (age == 16) {
    data <- data %>%
      select(NSID, W3alceverYP) %>%
      rename(alcever = W3alceverYP)
  } else if (age == 17) {
    data <- data %>%
      select(NSID, W4AlcEverYP) %>%
      rename(alcever = W4AlcEverYP)
  } else if (age == 19) {
    data <- data %>%
      select(NSID, W6AlcEverYP) %>%
      rename(alcever = W6AlcEverYP)
  } else if (age == 20) {
    data <- data %>%
      select(NSID, W7AlcEverYP) %>%
      rename(alcever = W7AlcEverYP)
  } else if (age == 25) {
    data <- data %>%
      select(NSID, W8AUDIT6) %>%
      rename(alcfreq = W8AUDIT6)
  } else if (age == 32) {
    data <- data %>%
      select(NSID, W9AUDIT1) %>%
      rename(alcfreq = W9AUDIT1)
  }

  data$age <- age
  return(data)
}

# Load all datasets
data_list <- lapply(names(file_paths), function(name) {
  load_and_process(file_paths[[name]], ages[[name]])
})

# Merge all datasets
merged_data <- bind_rows(data_list)

# Calculate drinker status for each row
merged_data <- merged_data %>%
  mutate(drinker_status = case_when(
    age == 14 ~ (!is.na(alcever) & alcever == 1 & !is.na(alcmon) & alcmon == 1),
    age %in% c(25, 32) ~ (!is.na(alcfreq) & alcfreq != 1),
    TRUE ~ (!is.na(alcever) & alcever == 1)
  ))

# Derive alcfst variable
result <- merged_data %>%
  group_by(NSID) %>%
  summarise(
    alcfst = {
      drinker_rows <- which(drinker_status)
      if (length(drinker_rows) > 0) {
        min_age <- min(age[drinker_rows])
        if (!is.finite(min_age)) NA_integer_ else min_age
      } else if (all(!is.na(drinker_status))) {
        99
      } else {
        -8
      }
    },
    .groups = "drop"
  )

# Convert alcfst to a factor with appropriate labels
result$alcfst <- factor(
  result$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c(
    "14", "15", "16", "17", "19", "20", "25", "32",
    "Never had alcohol", "Don't know/insufficient information"
  )
)

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

# Write the output to CSV
write.csv(result, "data/output/cleaned_data.csv", row.names = FALSE)

# Debug: Print first few rows of the result
cat("First few rows of result:\n")
print(head(result))
