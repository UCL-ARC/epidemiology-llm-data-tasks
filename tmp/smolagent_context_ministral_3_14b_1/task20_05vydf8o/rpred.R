# Load required libraries
library(readr)
library(dplyr)
library(tidyr)

# Load all input files with explicit column specification
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols(NSID = col_character()))

# Define mapping for sweep to age
sweep_age_map <- tibble(
  sweep = c(1, 2, 3, 4, 6, 7, 8, 9),
  age = c(14, 15, 16, 17, 19, 20, 25, 32)
)

# Merge all datasets by NSID and add sweep and age columns
merged_data <- bind_rows(
  wave1 %>% mutate(sweep = 1),
  wave2 %>% mutate(sweep = 2),
  wave3 %>% mutate(sweep = 3),
  wave4 %>% mutate(sweep = 4),
  wave6 %>% mutate(sweep = 6),
  wave7 %>% mutate(sweep = 7),
  wave8 %>% mutate(sweep = 8),
  wave9 %>% mutate(sweep = 9)
) %>% 
  left_join(sweep_age_map, by = 'sweep')

# Create drinking status variable with proper handling
merged_data <- merged_data %>%
  mutate(
    drank = case_when(
      sweep == 1 ~ (!is.na(W1alceverYP) & !is.na(W1alcmonYP) & W1alceverYP == 1 & W1alcmonYP == 1),
      sweep == 2 ~ (!is.na(W2alceverYP) & W2alceverYP == 1),
      sweep == 3 ~ (!is.na(W3alceverYP) & W3alceverYP == 1),
      sweep == 4 ~ (!is.na(W4AlcEverYP) & W4AlcEverYP == 1),
      sweep == 6 ~ (!is.na(W6AlcEverYP) & W6AlcEverYP == 1),
      sweep == 7 ~ (!is.na(W7AlcEverYP) & W7AlcEverYP == 1),
      sweep == 8 ~ (!is.na(W8AUDIT1) & W8AUDIT1 > 1),
      sweep == 9 ~ (!is.na(W9AUDIT1) & W9AUDIT1 > 1),
      TRUE ~ FALSE
    )
  )

# Determine earliest age of drinking
alc_fst <- merged_data %>%
  group_by(NSID) %>%
  summarise(
    alcfst = if(any(drank)) {
      min(age[drank])
    } else if (all(!drank) & any(is.na(drank))) {
      -8
    } else {
      99
    },
    .groups = 'drop'
  )

# Get all unique NSIDs to ensure all members are included
all_nsids <- merged_data %>% distinct(NSID)

# Merge the result back to include all NSIDs
result <- all_nsids %>% 
  left_join(alc_fst, by = 'NSID')

# Create factor with appropriate levels and labels
result$alcfst <- factor(
  result$alcfst,
  levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
  labels = c("Age 14", "Age 15", "Age 16", "Age 17", "Age 19",
             "Age 20", "Age 25", "Age 32", "Never had alcohol", "Don't know/insufficient information")
)

# Write the final output
write_csv(result, 'data/output/cleaned_data.csv', na = "")

# Print confirmation message
cat("Processing complete. Output written to data/output/cleaned_data.csv\n")