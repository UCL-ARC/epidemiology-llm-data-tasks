library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Define file paths
base_path <- 'data/input/'

# Load wave 8 main interview data (for educ25 - vocational qualifications)
w8_main <- read_delim(
  paste0(base_path, 'ns8_2015_main_interview.tab'),
  delim = '\t',
  col_types = cols(NSID = col_character())
)

# Load wave 8 derived data (for educ25 - academic NVQ)
w8_derived <- read_delim(
  paste0(base_path, 'ns8_2015_derived.tab'),
  delim = '\t',
  col_types = cols(NSID = col_character())
)

# Load wave 9 main interview data (for educadtl32 and educvdtl32)
w9_main <- read_delim(
  paste0(base_path, 'ns9_2022_main_interview.tab'),
  delim = '\t',
  col_types = cols(NSID = col_character())
)

# Load wave 9 derived variables (for educ32)
w9_derived <- read_delim(
  paste0(base_path, 'ns9_2022_derived_variables.tab'),
  delim = '\t',
  col_types = cols(NSID = col_character())
)

# Merge all datasets by NSID using full_join
merged_data <- w8_main %>%
  full_join(w8_derived, by = 'NSID') %>%
  full_join(w9_main, by = 'NSID') %>%
  full_join(w9_derived, by = 'NSID')

# Define standard missing codes
std_miss <- c(-9, -8, -3, -2, -1)

# Helper function to check if value is valid (not missing)
is_valid <- function(x) {
  !is.na(x) & !x %in% std_miss
}

# Function to classify wave 8 vocational qualifications to NVQ level
classify_w8_voc <- function(row) {
  # Variables that are NVQ 4-5 equivalent (check these first)
  # W8VCQU0E: Modern apprenticeship/trade = NVQ 4-5
  # W8VCQU0H: GNVQ/GSVQ = NVQ 4-5
  # W8VCQU0J: NVQ/SVQ Level 3-5 = NVQ 4-5
  # W8VCQU0K: HNC/HND = NVQ 4-5
  nvq45_vars <- c('W8VCQU0E', 'W8VCQU0H', 'W8VCQU0J', 'W8VCQU0K')
  
  # Variables that are NVQ 1-3 equivalent
  # W8VCQU0A-0C: Entry level = NVQ 1-3 equivalent
  # W8VCQU0D: Entry level (Wales) = NVQ 1-3 equivalent
  # W8VCQU0F: RSA/OCR/Clerical = NVQ 1-3 equivalent
  # W8VCQU0G: City and Guilds Certificate = NVQ 1-3 equivalent
  # W8VCQU0I: NVQ/SVQ Level 1-2 = NVQ 1-3 equivalent
  # W8VCQU0L: ONC/OND = NVQ 1-3 equivalent
  # W8VCQU0M: BTEC/BEC/TEC/EdExcel/LQL = NVQ 1-3 equivalent
  # W8VCQU0N: SCOTVEC, SCOTEC or SCOTBEC = NVQ 1-3 equivalent
  # W8VCQU0O: Other vocational = NVQ 1-3 equivalent
  nvq13_vars <- c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0F', 
                   'W8VCQU0G', 'W8VCQU0I', 'W8VCQU0L', 'W8VCQU0M', 
                   'W8VCQU0N', 'W8VCQU0O', 'W8VCQU0P')
  
  # Missing codes
  missing_codes <- c(-9, -8, -1, -2, -3)
  
  # Function to check if a variable is valid for a level
  check_valid <- function(var, level_vars) {
    for (v in level_vars) {
      val <- tryCatch(row[[v]], error = function(e) NA)
      if (!is.na(val) && val == 1) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # First check for NVQ 4-5
  if (check_valid(row, nvq45_vars)) {
    return(4)
  }
  
  # Then check for NVQ 1-3
  if (check_valid(row, nvq13_vars)) {
    return(1)
  }
  
  # Check academic NVQ from W8DHANVQH
  academic_niq <- row[['W8DHANVQH']]
  if (!is.na(academic_niq) && academic_niq %in% -9:-3) {
    # Convert academic NVQ to our 0-4 scale
    # 1=NVQ 1, 2=NVQ 2, 3=NVQ 3, 4=NVQ 4, 5=NVQ 5, 95=Other, 96=None
    if (academic_niq %in% c(1, 2, 3)) return(1)
    if (academic_niq %in% c(4, 5, 95)) return(4)
    if (academic_niq == 96) return(4)  # None of these
  }
  
  # Check for Missing or Not applicable (return level 3 for other qualifications)
  if (!is.na(academic_niq) && academic_niq %in% c(-9, -8, -1)) {
    return(3)  # Other qualifications not mappable
  }
  
  return(2)  # Entry level or no qualifications
}

# Derive educ25 directly as a column in merged_data
merged_data <- merged_data %>%
  mutate(
    educ25 = classify_w8_voc(.),
    across(all_of(c('NSID')), as.character)
  )

# Create the 5-level factor for educ25
merged_data <- merged_data %>%
  mutate(
    educ25 = factor(educ25,
      levels = c(0, 1, 2, 3, 4),
      labels = c('NVQ 4-5 equivalent', 'NVQ 1-3 equivalent', 'Entry level or no qualifications', 
                 'Other qualifications not mappable', 'None of these qualifications'),
      ordered = TRUE
    )
  )

# Derive educ32 from wave 9 derived variables
# W9DANVQH and W9DVNVQH are the two source variables
dev32_map <- function(row) {
  dan_niq <- row[['W9DANVQH']]
  dvv_niq <- row[['W9DVNVQH']]
  
  # Get valid values (not missing, not -1, -8, -9)
  dan_valid <- !is.na(dan_niq) && dan_niq %in% c(0, 1, 2, 3, 4, 5, 95, 96)
  dvv_valid <- !is.na(dvv_niq) && dvv_niq %in% c(0, 1, 2, 3, 4, 5, 95, 96)
  
  # If only one valid, use it
  if (dan_valid && !dvv_valid) {
    return(dan_niq)
  }
  if (!dan_valid && dvv_valid) {
    return(dvv_niq)
  }
  
  # If both valid, take maximum
  if (dan_valid && dvv_valid) {
    return(max(dan_niq, dvv_niq))
  }
  
  # Both missing - return missing code
  return(-3)
}

merged_data <- merged_data %>%
  mutate(
    educ32_raw = dev32_map(.)
  )

# Map educ32_raw to our 0-4 scale
merged_data <- merged_data %>%
  mutate(
    educ32 = case_when(
      educ32_raw %in% 1:3 ~ 1,
      educ32_raw %in% c(4, 5, 95) ~ 0,
      educ32_raw == 96 ~ 4,  # None of these qualifications
      educ32_raw == -3 ~ 4,  # None of these qualifications (missing)
      educ32_raw == 0 ~ 2,   # Entry level
      TRUE ~ 3  # Other missing
    )
  ) %>%
  mutate(
    educ32 = factor(educ32,
      levels = c(0, 1, 2, 3, 4),
      labels = c('NVQ 4-5 equivalent', 'NVQ 1-3 equivalent', 'Entry level or no qualifications', 
                 'Other qualifications not mappable', 'None of these qualifications'),
      ordered = TRUE
    )
  )

# Derive educadtl32 from W9ACQU* variables
w9acqu_vars <- c(
  'W9ACQU0A', 'W9ACQU0B', 'W9ACQU0C', 'W9ACQU0D', 'W9ACQU0E',
  'W9ACQU0F', 'W9ACQU0G', 'W9ACQU0H', 'W9ACQU0I', 'W9ACQU0J',
  'W9ACQU0K', 'W9ACQU0L', 'W9ACQU0M', 'W9ACQU0N', 'W9ACQU0O',
  'W9ACQU0P', 'W9ACQU0Q', 'W9ACQU0R', 'W9ACQU0S', 'W9ACQU0T',
  'W9ACQU0U', 'W9ACQU0V'
)

acqu_labels <- c(
  'Doctorate or equivalent', 'Masters or equivalent', 'Undergraduate or equivalent',
  'Post-graduate Diplomas and Certificates', 'Diplomas in higher education and other 
   higher education qualifications', 'Teaching qualifications for schools or further 
   education (below degree level)', 'A/AS Levels or equivalent', 'Grade A-C, Level 4-9',
  'Grade D-G, Level 1-3', 'SCE Higher', 'Scottish Certificate Sixth Year Studies',
  'SCE Standard', 'National 4 and 5', 'National 2 and 3', 'Leaving Certificate',
  'Junior Certificate grade A-C', 'Junior Certificate grade D and below', 'Other academic 
   qualifications (including overseas)', 'None of these qualifications', "Don't know",
  'Refused', 'No answer'
)

derive_acqu <- function(row) {
  for (i in seq_along(w9acqu_vars)) {
    var_name <- w9acqu_vars[i]
    val <- row[[var_name]]
    # Check for valid "Yes" response
    if (!is.na(val) && val == 1) {
      return(acqu_labels[i])
    }
  }
  # All responses are "No" or missing
  return('None of these qualifications')
}

merged_data <- merged_data %>%
  mutate(
    educadtl32 = derive_acqu(.)
  )

# Derive educvdtl32 from W9VCQU* variables
w9vcqu_vars <- c(
  'W9VCQU0A', 'W9VCQU0B', 'W9VCQU0C', 'W9VCQU0D', 'W9VCQU0E',
  'W9VCQU0F', 'W9VCQU0G', 'W9VCQU0H', 'W9VCQU0I', 'W9VCQU0J',
  'W9VCQU0K', 'W9VCQU0L', 'W9VCQU0M', 'W9VCQU0N', 'W9VCQU0O',
  'W9VCQU0P', 'W9VCQU0Q', 'W9VCQU0R', 'W9VCQU0S', 'W9VCQU0T',
  'W9VCQU0U', 'W9VCQU0V', 'W9VCQU0W', 'W9VCQU0X', 'W9VCQU0Y',
  'W9VCQU0Z', 'W9VCQUAA', 'W9VCQUAB', 'W9VCQUAC', 'W9VCQUAD',
  'W9VCQUAE', 'W9VCQUAF', 'W9VCQUAG', 'W9VCQUAH', 'W9VCQUAI'
)

vcqu_labels <- c(
  'Professional qualifications at degree level', 'Nursing or other medical qualifications (below degree level)',
  'Level 4 or 5', 'Level 3', 'Level 2', 'Level 1', 'GNVQ Advanced', 'GNVQ Intermediate',
  'Level 3', 'Level 2', 'Level Foundation', 'Advanced Craft, Part III', 'Craft, Part II',
  'Craft, Part I', 'Level 3', 'Level 2', 'Level 1', 'Advanced Diploma', 'Higher Diploma',
  'RSA Diploma', 'RSA Stage I, II,III', 'Higher Level BTEC', 'BTEC National', 'BTEC First',
  'SCOTVEC National Certificate', 'SCOTVEC first or general diploma', 'SCOTVEC general diploma',
  'SCOTVEC modules', 'HND or HNC', 'OND or ONCM', 'Junior certificate',
  'Other vocational qualifications (including some overseas)', 'None of these qualifications',
  "Don't know", 'Refused'
)

derive_vcqu <- function(row) {
  for (i in seq_along(w9vcqu_vars)) {
    var_name <- w9vcqu_vars[i]
    val <- row[[var_name]]
    # Check for valid "Yes" response
    if (!is.na(val) && val == 1) {
      return(vcqu_labels[i])
    }
  }
  # All responses are "No" or missing
  return('None of these qualifications')
}

merged_data <- merged_data %>%
  mutate(
    educvdtl32 = derive_vcqu(.)
  )

# Select only required variables
final_data <- merged_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Script completed successfully. Output written to data/output/cleaned_data.csv\n')
cat('Rows:', nrow(final_data), '\n')
cat('Columns:', names(final_data), '\n')

# Check factor structures
cat('\nFactor structures:\n')
print(summary(final_data$educ25))
print(summary(final_data$educ32))