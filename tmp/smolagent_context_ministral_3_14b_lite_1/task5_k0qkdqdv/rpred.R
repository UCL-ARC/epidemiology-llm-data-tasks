
# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Load metadata into a list for easy access
metadata <- list(
  wave_one = list(
    file = 'data/input/wave_one_lsype_young_person_2020.tab',
    id_var = 'NSID',
    target_vars = NULL
  ),
  wave_four = list(
    file = 'data/input/wave_four_lsype_young_person_2020.tab',
    id_var = 'NSID',
    target_vars = NULL
  ),
  wave_six = list(
    file = 'data/input/wave_six_lsype_young_person_2020.tab',
    id_var = 'NSID',
    target_vars = list(
      W6MarStatYP = list(
        pos = 570,
        label = 'YP: Marital status',
        user_missing = c('-999.0', '-997.0', '-97.0', '-92.0', '-91.0', '-1.0'),
        value_labels = c(
          '-997.0' = 'Script error',
          '-97.0' = 'Respondent declined self completion',
          '-92.0' = 'Refused',
          '-91.0' = 'Not applicable',
          '-1.0' = "Don't know",
          '1.0' = 'Single, that is never married',
          '2.0' = 'Married',
          '3.0' = 'Separated',
          '4.0' = 'Divorced',
          '5.0' = 'Widowed'
        )
      )
    )
  ),
  ns8 = list(
    file = 'data/input/ns8_2015_derived.tab',
    id_var = 'NSID',
    target_vars = list(
      W8DMARSTAT = list(
        pos = 10,
        label = 'DV: Legal marital status',
        user_missing = c('-9.0', '-8.0', '-1.0'),
        value_labels = c(
          '-9.0' = 'Refused',
          '-8.0' = 'Insufficient information',
          '-1.0' = 'Not applicable',
          '1.0' = 'Single and never married or in a CP',
          '2.0' = 'Married',
          '3.0' = 'Separated but still legally married',
          '4.0' = 'Divorced',
          '5.0' = 'Widowed',
          '6.0' = 'A Civil Partner',
          '7.0' = 'Separated but still legally in a CP',
          '8.0' = 'A former Civil Partner',
          '9.0' = 'A surviving Civil Partner'
        )
      )
    )
  ),
  ns9 = list(
    file = 'data/input/ns9_2022_derived_variables.tab',
    id_var = 'NSID',
    target_vars = list(
      W9DMARSTAT = list(
        pos = 16,
        label = 'DV: Legal marital status',
        user_missing = c('-8.0', '-9.0'),
        value_labels = c(
          '-9.0' = 'Refused',
          '-8.0' = 'Insufficient information',
          '1.0' = 'Single that is never married or never in a Civil Partnership',
          '2.0' = 'Married',
          '3.0' = 'Divorced',
          '4.0' = 'Legally separated',
          '5.0' = 'Widowed',
          '6.0' = 'A Civil Partner in a legally recognised Civil Partnership',
          '7.0' = 'A former Civil Partner (where Civil Partnership legally dissolved)',
          '8.0' = 'A surviving Civil Partner (where Civil Partner has died)'
        )
      )
    )
  )
)

# Load all datasets
load_datasets <- function() {
  datasets <- map(metadata, ~ {
    file_path <- .x$file
    id_var <- .x$id_var

    # Load the file
    data <- read_delim(file_path, delim = '\t')

    # Rename NSID to match metadata
    data <- rename(data, NSID = id_var)

    return(data)
  })

  # Merge datasets by NSID
  merged_data <- datasets[[1]]
  for (i in 2:length(datasets)) {
    merged_data <- full_join(merged_data, datasets[[i]], by = 'NSID')
  }

  return(merged_data)
}

# Process missing values and harmonise variables
harmonise_marital_status <- function(data) {
  # Define mapping for missing values
  missing_mapping <- list(
    '-997.0' = '-2', '-97.0' = '-2', '-92.0' = '-9', '-91.0' = '-1', '-1.0' = '-8',
    '-9.0' = '-9', '-8.0' = '-8', '-1.0' = '-1'
  )

  # Define mapping for marital status categories
  category_mapping <- list(
    W6MarStatYP = c(
      '1.0' = '1', '2.0' = '2', '3.0' = '3', '4.0' = '4', '5.0' = '5',
      '-999.0' = '-2', '-997.0' = '-2', '-97.0' = '-2', '-92.0' = '-9', '-91.0' = '-1', '-1.0' = '-8'
    ),
    W8DMARSTAT = c(
      '1.0' = '1', '2.0' = '2', '3.0' = '3', '4.0' = '4', '5.0' = '5',
      '6.0' = '1', '7.0' = '3', '8.0' = '4', '9.0' = '5',
      '-9.0' = '-9', '-8.0' = '-8', '-1.0' = '-1'
    ),
    W9DMARSTAT = c(
      '1.0' = '1', '2.0' = '2', '3.0' = '4', '4.0' = '3', '5.0' = '5',
      '6.0' = '2', '7.0' = '4', '8.0' = '5',
      '-9.0' = '-9', '-8.0' = '-8'
    )
  )

  # Create new variables for each age
  data <- data %>%
    mutate(
      partnr19 = recode(W6MarStatYP, !!!category_mapping$W6MarStatYP),
      partnr25 = recode(W8DMARSTAT, !!!category_mapping$W8DMARSTAT),
      partnr32 = recode(W9DMARSTAT, !!!category_mapping$W9DMARSTAT)
    ) %>%
    # Create detailed adult versions (partnradu25, partnradu32)
    mutate(
      partnradu25 = W8DMARSTAT,
      partnradu32 = W9DMARSTAT
    ) %>%
    # Convert NA to -3 (Not asked)
    mutate(
      partnr19 = ifelse(is.na(partnr19), -3, partnr19),
      partnr25 = ifelse(is.na(partnr25), -3, partnr25),
      partnr32 = ifelse(is.na(partnr32), -3, partnr32),
      partnradu25 = ifelse(is.na(partnradu25), -3, partnradu25),
      partnradu32 = ifelse(is.na(partnradu32), -3, partnradu32)
    )

  # Define labels for partnr variables
  partnr_labels <- c(
    '-9' = 'Refusal',
    '-8' = 'Insufficient information',
    '-7' = 'Prefer not to say',
    '-3' = 'Not asked',
    '-2' = 'Schedule not applicable / script error',
    '-1' = 'Item not applicable',
    '1' = 'Single',
    '2' = 'Married',
    '3' = 'Separated',
    '4' = 'Divorced',
    '5' = 'Widowed'
  )

  # Apply labels to partnr variables
  data <- data %>%
    mutate(
      partnr19 = factor(partnr19, levels = names(partnr_labels), labels = partnr_labels),
      partnr25 = factor(partnr25, levels = names(partnr_labels), labels = partnr_labels),
      partnr32 = factor(partnr32, levels = names(partnr_labels), labels = partnr_labels)
    )

  return(data)
}

# Main execution
data <- load_datasets()
cleaned_data <- harmonise_marital_status(data)

# Select only the ID and final derived variables
output_vars <- c('NSID', 'partnr19', 'partnr25', 'partnr32', 'partnradu25', 'partnradu32')
cleaned_data <- cleaned_data %>% select(all_of(output_vars))

# Write output
write_csv(cleaned_data, 'data/output/cleaned_data.csv')

# Print success message
message('Data cleaning and preprocessing complete. Output saved to data/output/cleaned_data.csv.')
