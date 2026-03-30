library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create directories if they don't exist
if (!dir.exists('data/input/')) {
  dir.create('data/input/', recursive = TRUE)
}

if (!dir.exists('data/output/')) {
  dir.create('data/output/', recursive = TRUE)
}

# Define file paths and their wave numbers as character keys
file_wave_map <- list(
  'wave_one_lsype_family_background_2020.tab' = '14',
  'wave_two_lsype_family_background_2020.tab' = '15',
  'wave_three_lsype_family_background_2020.tab' = '16',
  'wave_four_lsype_family_background_2020.tab' = '17', 
  'wave_five_lsype_family_background_2020.tab' = '18'
)

# Define variable mappings by wave number as character keys
var_mappings_list <- list(
  '14' = list(mum = 'W1nsseccatmum', dad = 'W1nsseccatdad'),
  '15' = list(mum = 'W2nsseccatmum', dad = 'W2nsseccatdad'),
  '16' = list(mum = 'W3cnsseccatmum', dad = 'W3cnsseccatdad'),
  '17' = list(mum = 'w4cnsseccatmum', dad = 'w4cnsseccatdad'),
  '18' = list(mum = 'w5Cnsseccatmum', dad = 'w5Cnsseccatdad')
)

# Function to harmonize missing value codes
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  
  # Recode special user missing values
  x[x == -999] <- -2
  x[x == -94] <- -8
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[is.na(x)] <- -3
  
  return(x)
}

# Function to create harmonized NS-SEC variable with labels
create_harmonized_nssec <- function(x, wave_label) {
  x <- as.integer(x)
  x <- harmonize_missing(x)
  
  # Create labels
  labels <- c(
    '1' = 'Employers in large organisations',
    '2' = 'Higher managerial occupations',
    '3' = 'Higher professional occupations',
    '4' = 'Lower professional occupations',
    '5' = 'Lower managerial occupations',
    '6' = 'Higher supervisory occupations',
    '7' = 'Intermediate occupations',
    '8' = 'Employers in small orgs non-professional',
    '9' = 'Own account workers non professional',
    '10' = 'Lower supervisory occupations',
    '11' = 'Lower technical craft',
    '12' = 'Semi routine occupations',
    '13' = 'Routine occupations',
    '14' = 'Never worked',
    '15' = 'Full-time students / Long-term unemployed',
    '16' = 'Not classified or inadequately stated',
    '17' = 'Not classifiable for other reasons',
    '-9' = 'Refusal',
    '-8' = "Don't know / Insufficient information",
    '-7' = 'Prefer not to say',
    '-3' = 'Not asked / Not interviewed',
    '-2' = 'Script error / Information lost',
    '-1' = 'Not applicable'
  )
  
  x <- factor(x, levels = as.numeric(names(labels)), labels = labels)
  
  # Set variable label using attr
  attr(x, 'label') <- paste0('NS-SEC occupational class (', wave_label, ')')
  
  return(x)
}

# Process each wave
processed_list <- list()

for (file_name in names(file_wave_map)) {
  wave_num <- file_wave_map[[file_name]]
  
  cat('Processing:', file_name, '\n')
  cat('Wave number:', wave_num, '\n')
  
  file_path <- paste0('data/input/', file_name)
  
  # Read the file as character
  df <- read_delim(file_path, delim = '\t', col_types = cols(.default = col_character()))
  
  cat('Columns in df:', length(names(df)), 'columns\n')
  
  # Get variable mappings for this wave
  var_map <- var_mappings_list[[wave_num]]
  
  cat('Mum var:', var_map$mum, 'Dad var:', var_map$dad, '\n')
  
  # Process mother and father NS-SEC variables
  old_mum_var <- var_map$mum
  old_dad_var <- var_map$dad
  
  if (old_mum_var %in% names(df)) {
    df[[paste0('nssecma', wave_num)]] <- create_harmonized_nssec(df[[old_mum_var]], paste0('Wave', wave_num))
    cat('Created nssecma', wave_num, '\n')
  }
  
  if (old_dad_var %in% names(df)) {
    df[[paste0('nssecpa', wave_num)]] <- create_harmonized_nssec(df[[old_dad_var]], paste0('Wave', wave_num))
    cat('Created nssecpa', wave_num, '\n')
  }
  
  cat('Columns after processing:', length(names(df)), 'columns\n')
  
  # Keep only the NS-SEC variables and NSID
  df <- df %>% select(NSID, all_of(c(paste0('nssecma', wave_num), paste0('nssecpa', wave_num))))
  
  cat('Final columns for wave', wave_num, ':', paste(names(df), collapse = ', '), '\n')
  
  processed_list[[file_name]] <- df
}

# Merge all waves using full_join by NSID
final_data <- NULL
for (i in seq_along(processed_list)) {
  if (is.null(final_data)) {
    final_data <- processed_list[[i]]
  } else {
    final_data <- full_join(final_data, processed_list[[i]], by = 'NSID')
  }
}

# Keep only the required variables
final_data <- final_data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Cleaning complete. Output written to data/output/cleaned_data.csv\n')
cat('Shape:', dim(final_data), '\n')
