
# Load required packages explicitly
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)
library(readr)

# Set options to suppress messages
options(warn = -1)

# Function to safely load and process files
process_data <- function() {
  tryCatch({
    # Load all files explicitly with error handling
    files <- c(
      'wave_one_lsype_young_person_2020.tab',
      'wave_two_lsype_young_person_2020.tab',
      'wave_three_lsype_young_person_2020.tab',
      'wave_four_lsype_young_person_2020.tab',
      'wave_five_lsype_young_person_2020.tab',
      'wave_six_lsype_young_person_2020.tab',
      'wave_seven_lsype_young_person_2020.tab',
      'ns8_2015_main_interview.tab',
      'ns9_2022_main_interview.tab'
    )

    data_frames <- lapply(files, function(file) {
      path <- paste0('data/input/', file)
      tryCatch({
        read_delim(path, delim = '\t')
      }, error = function(e) {
        message(paste('Error loading', file, ': ', e$message))
        return(NULL)
      })
    })

    # Remove NULL entries (failed loads)
    data_frames <- data_frames[!sapply(data_frames, is.null)]

    # Merge all datasets by NSID
    merged_data <- data_frames[[1]]
    for (df in data_frames[-1]) {
      merged_data <- full_join(merged_data, df, by = 'NSID')
    }

    # Define sex variables and their mappings
    sex_mapping <- list(
      W1sexYP = list(c(-99.0, -92.0, -91.0, 1.0, 2.0), c(-3, -9, -1, 1, 2)),
      W2SexYP = list(c(-998.0, -997.0, -995.0, -99.0, -92.0, -91.0, -1.0, 1.0, 2.0), c(-2, -2, -2, -3, -9, -1, -8, 1, 2)),
      W3sexYP = list(c(-99.0, -92.0, -91.0, 1.0, 2.0), c(-3, -9, -1, 1, 2)),
      W4SexYP = list(c(-99.0, -92.0, -91.0, -1.0, 1.0, 2.0), c(-3, -9, -1, -8, 1, 2)),
      W5SexYP = list(c(-1.0, 1.0, 2.0), c(-8, 1, 2)),
      W6Sex = list(c(-92.0, -91.0, 1.0, 2.0), c(-9, -1, 1, 2)),
      W7Sex = list(c(-91.0, 1.0, 2.0), c(-1, 1, 2)),
      W8CMSEX = list(c(-9.0, -8.0, -1.0, 1.0, 2.0), c(-9, -8, -1, 1, 2)),
      W9DSEX = list(c(1.0, 2.0), c(1, 2))
    )

    # Function to recode variables
    recode_variable <- function(data, var_name, mapping) {
      if (!var_name %in% names(data)) return(data)
      old_codes <- mapping[[1]]
      new_codes <- mapping[[2]]
      data[[var_name]] <- as.numeric(data[[var_name]])
      for (i in seq_along(old_codes)) {
        data[[var_name]][data[[var_name]] == old_codes[i]] <- new_codes[i]
      }
      data[[var_name]][is.na(data[[var_name]])] <- -3
      return(data)
    }

    # Apply recoding to each sex variable
    for (var in names(sex_mapping)) {
      if (var %in% names(merged_data)) {
        merged_data <- recode_variable(merged_data, var, sex_mapping[[var]])
      }
    }

    # Consolidate sex variable using earliest-valid-first rule
    sex_data <- merged_data %>%
      transmute(
        NSID = NSID,
        sex = coalesce(
          W1sexYP,
          W2SexYP,
          W3sexYP,
          W4SexYP,
          W5SexYP,
          W6Sex,
          W7Sex,
          W8CMSEX,
          W9DSEX
        )
      )

    # Create labeled factor for sex
    sex_data$sex <- factor(
      sex_data$sex,
      levels = c(-9, -8, -7, -3, -2, -1, 1, 2),
      labels = c('Refusal', 'Don\'t know', 'Prefer not to say', 'Not interviewed',
                 'Schedule not applicable', 'Not applicable', 'Male', 'Female')
    )

    # Write the output to CSV
    write_csv(sex_data, 'data/output/cleaned_data.csv')

    # Print confirmation
    message('Data processing completed successfully.')
    message('Output file written to: data/output/cleaned_data.csv')
  }, error = function(e) {
    message(paste('Error during processing:', e$message))
  })
}

# Run the processing
process_data()
