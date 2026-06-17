library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Create list of dataframes with wave info
data_list <- list(
  wave1 = wave1,
  wave2 = wave2,
  wave3 = wave3,
  wave4 = wave4,
  wave6 = wave6,
  wave7 = wave7,
  wave8 = wave8,
  wave9 = wave9
)

# Map wave names to variables and ages
wave_names <- c('wave1', 'wave2', 'wave3', 'wave4', 'wave6', 'wave7', 'wave8', 'wave9')
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
wave_vars <- c('W1alceverYP', 'W2alceverYP', 'W3alceverYP', 'W4AlcEverYP', 'W6AlcEverYP', 'W7AlcEverYP', 'W8AUDIT1', 'W9AUDIT1')

# Recode each wave's ever-drank variable
results <- list()
for (i in seq_along(wave_names)) {
  wave_name <- wave_names[i]
  age <- ages[i]
  var_name <- wave_vars[i]
  
  df <- data_list[[wave_name]]
  ever_val <- df[[var_name]]
  
  result <- data.frame(
    NSID = df$NSID,
    alc_ever = ifelse(ever_val == 1, 1, NA),
    alc_age = ifelse(ever_val == 1, age, NA),
    stringsAsFactors = FALSE
  )
  results[[wave_name]] <- result
}

# Combine all results
combined <- bind_rows(results)

# Find earliest age with alc_ever == 1, or 99 if none
alcfst <- combined %>%
  group_by(NSID) %>%
  summarise(alcfst = {
    valid_ages <- alc_age[alc_ever == 1]
    if (length(valid_ages) > 0 && !all(is.na(valid_ages))) {
      min(valid_ages, na.rm = TRUE)
    } else {
      99
    }
  }, .groups = 'drop')

write_csv(alcfst, 'data/output/cleaned_data.csv')
print('Done')
print(head(alcfst))
print(table(alcfst$alcfst))