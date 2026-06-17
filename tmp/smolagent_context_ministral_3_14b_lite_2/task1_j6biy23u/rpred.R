
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Define file paths and column positions
files_info <- list(
  wave_one = list(file = 'wave_one_lsype_young_person_2020.tab', var_pos = 92),
  wave_two = list(file = 'wave_two_lsype_young_person_2020.tab', var_pos = 84),
  wave_three = list(file = 'wave_three_lsype_young_person_2020.tab', var_pos = 60),
  wave_four = list(file = 'wave_four_lsype_young_person_2020.tab', var_pos = 56),
  wave_five = list(file = 'wave_five_lsype_young_person_2020.tab', var_pos = 8),
  wave_six = list(file = 'wave_six_lsype_young_person_2020.tab', var_pos = 10),
  wave_seven = list(file = 'wave_seven_lsype_young_person_2020.tab', var_pos = 6),
  wave_eight = list(file = 'ns8_2015_main_interview.tab', var_pos = 9),
  wave_nine = list(file = 'ns9_2022_main_interview.tab', var_pos = 8)
)

# Function to clean sex data
clean_sex <- function(x) {
  x[is.na(x)] <- -3
  x[x %in% c(-999, -998, -997, -995, -99)] <- -3
  x[x %in% c(-92, -9)] <- -9
  x[x %in% c(-91, -1, -8)] <- -1
  return(x)
}

# Create empty list to store all sex data
sex_data <- list()

# Load and process each file
for (i in seq_along(files_info)) {
  file_info <- files_info[[i]]
  tryCatch({
    # Read the file
    data <- read_delim(paste0('data/input/', file_info$file), delim = '\t', show_col_types = FALSE)

    # Extract NSID (first column) and sex variable (specified position)
    nsid <- data[[1]]
    sex_var <- data[[file_info$var_pos]]

    # Clean the sex variable
    cleaned_sex <- clean_sex(sex_var)

    # Store as data frame with proper names
    sex_data[[paste0('wave', i)]] <- data.frame(
      NSID = nsid,
      sex = cleaned_sex
    )
  }, error = function(e) {
    # If file fails to load, create empty data frame
    sex_data[[paste0('wave', i)]] <- data.frame(
      NSID = character(0),
      sex = numeric(0)
    )
  })
}

# Get all unique NSIDs from the first wave
if (length(sex_data) > 0) {
  first_wave <- names(sex_data)[1]
  all_nsids <- sex_data[[first_wave]] %>% distinct(NSID)
} else {
  all_nsids <- data.frame(NSID = character(0))
}

# Create complete dataset with all NSIDs
complete_data <- all_nsids

# Add each sex variable to the complete dataset
for (wave_name in names(sex_data)) {
  if (nrow(sex_data[[wave_name]]) > 0) {
    complete_data <- complete_data %>%
      left_join(sex_data[[wave_name]], by = 'NSID') %>%
      rename({{wave_name}} := sex)
  } else {
    complete_data[[wave_name]] <- rep(-3, nrow(complete_data))
  }
}

# Determine most recent valid sex value
consolidated_sex <- apply(select(complete_data, -NSID), 1, function(x) {
  valid_values <- x[!is.na(x) & x != -3 & x != -9 & x != -1]
  if (length(valid_values) > 0) {
    return(max(valid_values, na.rm = TRUE))
  } else {
    return(-3)
  }
})

# Create final output
final_data <- complete_data %>%
  mutate(sex = factor(consolidated_sex,
                     levels = c(-3, -9, -1, 1, 2),
                     labels = c('Missing', 'Refused', 'Not applicable', 'Male', 'Female'))) %>%
  select(NSID, sex)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')
