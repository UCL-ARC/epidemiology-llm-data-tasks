
# Load required libraries
library(readr)
library(dplyr)

# Define file paths
files <- c(
  'data/input/wave_one_lsype_young_person_2020.tab',
  'data/input/wave_two_lsype_young_person_2020.tab',
  'data/input/wave_three_lsype_young_person_2020.tab',
  'data/input/wave_four_lsype_young_person_2020.tab',
  'data/input/wave_six_lsype_young_person_2020.tab',
  'data/input/wave_seven_lsype_young_person_2020.tab',
  'data/input/ns8_2015_self_completion.tab',
  'data/input/ns9_2022_main_interview.tab'
)

# Load all files and merge them
data_list <- lapply(files, function(file) {
  read_delim(file, delim = '\t', col_types = cols(NSID = col_character()))
})

merged_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  merged_data <- full_join(merged_data, data_list[[i]], by = 'NSID')
}

# Define the mapping for alcfst
alc_vars <- list(
  W1alceverYP = 14,
  W2alceverYP = 15,
  W3alceverYP = 16,
  W4AlcEverYP = 17,
  W6AlcEverYP = 19,
  W7AlcEverYP = 20,
  W8AUDIT1 = 25,
  W9AUDIT1 = 32
)

# Function to determine alcfst for a single variable
get_alcfst <- function(data, var_name, age) {
  if (var_name %in% names(data)) {
    # For W8AUDIT1 and W9AUDIT1: 1=Never(99), 2=Monthly or less(age)
    if (grepl('W8AUDIT1|W9AUDIT1', var_name)) {
      return(ifelse(data[[var_name]] == 1, 99,
                    ifelse(data[[var_name]] == 2, age, NA)))
    }
    # For other waves: 1=Yes(age), 2=No(99)
    else {
      return(ifelse(data[[var_name]] == 1, age,
                    ifelse(data[[var_name]] == 2, 99, NA)))
    }
  }
  return(NA)
}

# Create alcfst columns for each wave
for (var_name in names(alc_vars)) {
  age <- alc_vars[[var_name]]
  col_name <- paste0('alcfst_', age)
  merged_data[[col_name]] <- get_alcfst(merged_data, var_name, age)
}

# Find all alcfst columns
alcfst_cols <- grep('^alcfst_', names(merged_data), value = TRUE)

# If no alcfst columns were created, create a default one
if (length(alcfst_cols) == 0) {
  merged_data$alcfst <- 99
} else {
  # Determine the earliest age of alcohol consumption
  alcfst_data <- merged_data %>%
    select(NSID, all_of(alcfst_cols)) %>%
    pivot_longer(cols = all_of(alcfst_cols),
                 names_to = 'age_var',
                 values_to = 'value') %>%
    mutate(age = as.numeric(gsub('alcfst_', '', age_var))) %>%
    arrange(NSID, age) %>%
    group_by(NSID) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(alcfst = ifelse(is.na(value), 99, value))

  merged_data <- merged_data %>%
    left_join(alcfst_data %>% select(NSID, alcfst), by = 'NSID')
}

# Select only NSID and alcfst for the final output
final_data <- merged_data %>% select(NSID, alcfst)

# Write the final output
write_csv(final_data, 'data/output/cleaned_data.csv')
