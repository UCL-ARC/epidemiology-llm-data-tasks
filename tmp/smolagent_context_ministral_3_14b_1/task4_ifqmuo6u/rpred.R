
# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define file paths
input_path <- "data/input/"
output_path <- "data/output/"

# Load metadata and define source variables
metadata <- list(
  wave_six = list(
    file = "wave_six_lsype_young_person_2020.tab",
    var = "W6SexualityYP",
    age = 19,
    mapping = list(
      '-97.0' = -9,  # Refusal
      '-92.0' = -9,  # Refused
      '-91.0' = -1,  # Not applicable
      '-1.0' = -8,   # Don't know
      '1.0' = 1,     # Heterosexual / Straight
      '2.0' = 2,     # Gay / Lesbian
      '3.0' = 3,     # Bisexual
      '4.0' = 4      # Other
    )
  ),
  wave_seven = list(
    file = "wave_seven_lsype_young_person_2020.tab",
    var = "W7SexualityYP",
    age = 20,
    mapping = list(
      '-100.0' = -9, # Respondent declined sexual experience questions
      '-97.0' = -9,  # Refused self completion
      '-92.0' = -9,  # Refused
      '-91.0' = -1,  # Not applicable
      '-1.0' = -8,   # Don't know
      '1.0' = 1,     # Heterosexual / Straight
      '2.0' = 2,     # Gay / Lesbian
      '3.0' = 3,     # Bisexual
      '4.0' = 4      # Other
    )
  ),
  wave_eight = list(
    file = "ns8_2015_self_completion.tab",
    var = "W8SEXUALITY",
    age = 25,
    mapping = list(
      '-9.0' = -9,   # Refused
      '-8.0' = -8,   # Don't know
      '-1.0' = -1,   # Not applicable
      '1.0' = 1,     # Heterosexual / Straight
      '2.0' = 2,     # Gay / Lesbian
      '3.0' = 3,     # Bisexual
      '4.0' = 4      # Other
    )
  ),
  wave_nine = list(
    file = "ns9_2022_main_interview.tab",
    var = "W9SORI",
    age = 32,
    mapping = list(
      '-9.0' = -9,   # Refused
      '-8.0' = -8,   # Don't know
      '-3.0' = -3,   # Not asked at fieldwork stage
      '-1.0' = -1,   # Not applicable
      '1.0' = 1,     # Heterosexual / Straight
      '2.0' = 2,     # Gay / Lesbian
      '3.0' = 3,     # Bisexual
      '4.0' = 4,     # Other
      '5.0' = -7     # Prefer not to say
    )
  )
)

# Load all datasets
load_datasets <- function() {
  files <- c(
    metadata$wave_six$file,
    metadata$wave_seven$file,
    metadata$wave_eight$file,
    metadata$wave_nine$file
  )

  datasets <- lapply(files, function(file) {
    readr::read_delim(paste0(input_path, file), delim = "\t")
  })

  names(datasets) <- c(
    "wave_six",
    "wave_seven",
    "wave_eight",
    "wave_nine"
  )

  return(datasets)
}

# Process and harmonize variables
process_data <- function(datasets) {
  processed_data <- datasets[[1]] %>% select(NSID)

  # Process each wave
  for (wave in names(datasets)) {
    current_data <- datasets[[wave]]
    current_var <- metadata[[wave]]$var
    current_mapping <- metadata[[wave]]$mapping
    current_age <- metadata[[wave]]$age

    # Rename the output variable
    output_var <- paste0("sori", current_age)

    # Map values according to metadata
    current_data <- current_data %>%
      mutate(!!output_var := recode(
        !!sym(current_var),
        !!!current_mapping
      )) %>%
      select(NSID, !!sym(output_var))

    # Merge with the main dataset
    processed_data <- processed_data %>%
      left_join(current_data, by = "NSID")
  }

  return(processed_data)
}

# Main execution
datasets <- load_datasets()
processed_data <- process_data(datasets)

# Write output
write_csv(processed_data, paste0(output_path, "cleaned_data.csv"))

# Print confirmation
cat("Data processing complete. Output saved to data/output/cleaned_data.csv\n")
