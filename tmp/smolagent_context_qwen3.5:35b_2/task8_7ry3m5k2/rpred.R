library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load files
df1 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
df2 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
df3 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)
df4 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets
merged <- full_join(df1, df2, by = 'NSID')
merged <- full_join(merged, df3, by = 'NSID')
merged <- full_join(merged, df4, by = 'NSID')

cat('Merged dataset dimensions:', nrow(merged), 'rows,', ncol(merged), 'columns\n')

# Function to map W8 vocational qualifications to NVQ levels
map_w8_voc_nvq <- function(var_name, value) {
  # Handle missing codes
  if (value %in% c(-9, -8, -3, -2, -1)) return(value)
  if (is.na(value)) return(NA_real_)
  if (value == 0) return(NA_real_)  # No qualification
  if (value == 1) {
    # Map based on variable name
    if (var_name %in% c('W8VCQU0J', 'W8VCQU0K')) {
      return(4)  # NVQ/SVQ Level 3-5, HNC/HND = NVQ 4-5
    } else if (var_name %in% c('W8VCQU0I', 'W8VCQU0L', 'W8VCQU0M', 'W8VCQU0N')) {
      return(3)  # NVQ/SVQ Level 1-2, ONC/OND, BTEC, SCOTVEC = NVQ 1-3
    } else if (var_name %in% c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 
                               'W8VCQU0E', 'W8VCQU0F', 'W8VCQU0G', 'W8VCQU0H')) {
      return(2)  # Lower level vocational = NVQ 1-3
    } else if (var_name %in% c('W8VCQU0O')) {
      return(95)  # Other vocational = Other qualifications
    } else if (var_name %in% c('W8VCQU0P')) {
      return(96)  # None of the above
    } else {
      return(NA_real_)
    }
  } else {
    return(NA_real_)
  }
}

# W8 vocational qualification variables
w8_voc_vars <- c('W8VCQU0A', 'W8VCQU0B', 'W8VCQU0C', 'W8VCQU0D', 'W8VCQU0E', 
                 'W8VCQU0F', 'W8VCQU0G', 'W8VCQU0H', 'W8VCQU0I', 'W8VCQU0J',
                 'W8VCQU0K', 'W8VCQU0L', 'W8VCQU0M', 'W8VCQU0N', 'W8VCQU0O',
                 'W8VCQU0P', 'W8VCQU0Q', 'W8VCQU0R')

# Create a function to get max NVQ level from vocational variables
get_max_voc_nvq <- function(row, vars) {
  max_nvq <- NA_real_
  for (var in vars) {
    val <- row[[var]]
    if (!is.na(val) && val != 0) {
      mapped <- map_w8_voc_nvq(var, val)
      if (!is.na(mapped)) {
        if (is.na(max_nvq) || mapped > max_nvq) {
          max_nvq <- mapped
        }
      }
    }
  }
  return(max_nvq)
}

# Apply to each row
merged$W8VOC_MAX <- sapply(1:nrow(merged), function(i) get_max_voc_nvq(merged[i, ], w8_voc_vars))

# First, map W8DHANVQH to NVQ levels
merged$W8DHANVQH_mapped <- case_when(
  merged$W8DHANVQH %in% c(4, 5) ~ 4,  # NVQ Level 4-5
  merged$W8DHANVQH %in% c(1, 2, 3) ~ merged$W8DHANVQH,  # NVQ Level 1-3
  merged$W8DHANVQH == 95 ~ 95,  # Other academic qualification
  merged$W8DHANVQH == 96 ~ 96,  # None of these qualifications
  TRUE ~ merged$W8DHANVQH  # Preserve missing codes
)

# Combine academic and vocational to get highest
merged$educ25_raw <- pmax(merged$W8DHANVQH_mapped, merged$W8VOC_MAX, na.rm = TRUE)

# Map to final collapsed categories
merged$educ25 <- case_when(
  merged$educ25_raw %in% c(4, 5) ~ 0,  # NVQ 4-5 equivalent
  merged$educ25_raw %in% c(0, 1, 2, 3) ~ 1,  # NVQ 1-3 equivalent
  merged$educ25_raw == 95 ~ 3,  # Other qualifications not mappable to NVQ framework
  merged$educ25_raw == 96 ~ 4,  # None of these qualifications
  TRUE ~ merged$educ25_raw  # Preserve missing codes
)

# Handle NA values (Null) as -3
merged$educ25[is.na(merged$educ25)] <- -3

# Convert to labelled factor
merged$educ25 <- factor(merged$educ25, 
                        levels = c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4),
                        labels = c('Refused', "Don't know", 'Null', 'Error', 'Not applicable', 
                                   'NVQ 4-5 equivalent', 'NVQ 1-3 equivalent', 'Entry level or no qualifications',
                                   'Other qualifications not mappable to NVQ framework', 'None of these qualifications'))

# Create educ32 from W9DANVQH and W9DVNVQH
merged$educ32_raw <- case_when(
  merged$W9DANVQH %in% c(4, 5) ~ 4,
  merged$W9DANVQH %in% c(0, 1, 2, 3) ~ merged$W9DANVQH,
  merged$W9DANVQH == 95 ~ 95,
  merged$W9DANVQH == 96 ~ 96,
  TRUE ~ merged$W9DANVQH
)

merged$educ32_raw <- pmax(merged$educ32_raw, merged$W9DVNVQH, na.rm = TRUE)

merged$educ32 <- case_when(
  merged$educ32_raw %in% c(4, 5) ~ 0,
  merged$educ32_raw %in% c(0, 1, 2, 3) ~ 1,
  merged$educ32_raw == 95 ~ 3,
  merged$educ32_raw == 96 ~ 4,
  TRUE ~ merged$educ32_raw
)

merged$educ32[is.na(merged$educ32)] <- -3

merged$educ32 <- factor(merged$educ32,
                        levels = c(-9, -8, -3, -2, -1, 0, 1, 2, 3, 4),
                        labels = c('Refused', "Don't know", 'Null', 'Error', 'Not applicable',
                                   'NVQ 4-5 equivalent', 'NVQ 1-3 equivalent', 'Entry level or no qualifications',
                                   'Other qualifications not mappable to NVQ framework', 'None of these qualifications'))

# Create educadtl32 from W9ACQU* variables
w9_acqu_vars <- c('W9ACQU0A', 'W9ACQU0B', 'W9ACQU0C', 'W9ACQU0D', 'W9ACQU0E',
                  'W9ACQU0F', 'W9ACQU0G', 'W9ACQU0H', 'W9ACQU0I', 'W9ACQU0J',
                  'W9ACQU0K', 'W9ACQU0L', 'W9ACQU0M', 'W9ACQU0N', 'W9ACQU0O',
                  'W9ACQU0P', 'W9ACQU0Q', 'W9ACQU0R', 'W9ACQU0S', 'W9ACQU0T',
                  'W9ACQU0U', 'W9ACQU0V')

# Create label mapping (stripping prefix)
acqu_labels <- c(
  'W9ACQU0A' = 'Doctorate or equivalent',
  'W9ACQU0B' = 'Masters or equivalent',
  'W9ACQU0C' = 'Undergraduate or equivalent',
  'W9ACQU0D' = 'Post-graduate Diplomas and Certificates',
  'W9ACQU0E' = 'Diplomas in higher education and other higher education qualifications',
  'W9ACQU0F' = 'Teaching qualifications for schools or further education (below degree level)',
  'W9ACQU0G' = 'A/AS Levels or equivalent',
  'W9ACQU0H' = 'Grade A-C, Level 4-9',
  'W9ACQU0I' = 'Grade D-G, Level 1-3',
  'W9ACQU0J' = 'SCE Higher',
  'W9ACQU0K' = 'Scottish Certificate Sixth Year Studies',
  'W9ACQU0L' = 'SCE Standard',
  'W9ACQU0M' = 'National 4 and 5',
  'W9ACQU0N' = 'National 2 and 3',
  'W9ACQU0O' = 'Leaving Certificate',
  'W9ACQU0P' = 'Junior Certificate grade A-C',
  'W9ACQU0Q' = 'Junior Certificate grade D and below',
  'W9ACQU0R' = 'Other academic qualifications (including overseas)',
  'W9ACQU0S' = 'None of these qualifications',
  'W9ACQU0T' = "Don't know",
  'W9ACQU0U' = 'Refused',
  'W9ACQU0V' = 'No answer'
)

# Function to find first Yes response
find_first_yes <- function(row, vars, labels) {
  for (var in vars) {
    val <- row[[var]]
    if (!is.na(val) && val == 1) {
      return(labels[var])
    }
  }
  return('None of these qualifications')
}

merged$educadtl32 <- mapply(function(i) {
  find_first_yes(merged[i, ], w9_acqu_vars, acqu_labels)
}, 1:nrow(merged))

# Create educvdtl32 from W9VCQU* variables
w9_voc_vars <- c('W9VCQU0A', 'W9VCQU0B', 'W9VCQU0C', 'W9VCQU0D', 'W9VCQU0E',
                 'W9VCQU0F', 'W9VCQU0G', 'W9VCQU0H', 'W9VCQU0I', 'W9VCQU0J',
                 'W9VCQU0K', 'W9VCQU0L', 'W9VCQU0M', 'W9VCQU0N', 'W9VCQU0O',
                 'W9VCQU0P', 'W9VCQU0Q', 'W9VCQU0R', 'W9VCQU0S', 'W9VCQU0T',
                 'W9VCQU0U', 'W9VCQU0V', 'W9VCQU0W', 'W9VCQU0X', 'W9VCQU0Y',
                 'W9VCQU0Z', 'W9VCQUAA', 'W9VCQUAB', 'W9VCQUAC', 'W9VCQUAD',
                 'W9VCQUAE', 'W9VCQUAF', 'W9VCQUAG', 'W9VCQUAH', 'W9VCQUAI')

# Create label mapping (stripping prefix)
voc_labels <- c(
  'W9VCQU0A' = 'Professional qualifications at degree level e.g. graduate member of professional institute, chartered accountant or surveyor',
  'W9VCQU0B' = 'Nursing or other medical qualifications (below degree level)',
  'W9VCQU0C' = 'Level 4 or 5',
  'W9VCQU0D' = 'Level 3',
  'W9VCQU0E' = 'Level 2',
  'W9VCQU0F' = 'Level 1',
  'W9VCQU0G' = 'GNVQ Advanced',
  'W9VCQU0H' = 'GNVQ Intermediate',
  'W9VCQU0I' = 'Level 3',
  'W9VCQU0J' = 'Level 2',
  'W9VCQU0K' = 'Level Foundation',
  'W9VCQU0L' = 'Advanced Craft, Part III',
  'W9VCQU0M' = 'Craft, Part II',
  'W9VCQU0N' = 'Craft, Part I',
  'W9VCQU0O' = 'Level 3',
  'W9VCQU0P' = 'Level 2',
  'W9VCQU0Q' = 'Level 1',
  'W9VCQU0R' = 'Advanced Diploma',
  'W9VCQU0S' = 'Higher Diploma',
  'W9VCQU0T' = 'RSA Diploma',
  'W9VCQU0U' = 'RSA Stage I, II,III',
  'W9VCQU0V' = 'Higher Level BTEC',
  'W9VCQU0W' = 'BTEC National',
  'W9VCQU0X' = 'BTEC First',
  'W9VCQU0Y' = 'SCOTVEC National Certificate',
  'W9VCQU0Z' = 'SCOTVEC first or general diploma',
  'W9VCQUAA' = 'SCOTVEC general diploma',
  'W9VCQUAB' = 'SCOTVEC modules',
  'W9VCQUAC' = 'HND or  HNC',
  'W9VCQUAD' = 'OND or ONCM',
  'W9VCQUAE' = 'Junior certificate',
  'W9VCQUAF' = 'Other vocational qualifications (including some overseas)',
  'W9VCQUAG' = 'None of these qualifications',
  'W9VCQUAH' = "Don't know",
  'W9VCQUAI' = 'Refused'
)

merged$educvdtl32 <- mapply(function(i) {
  find_first_yes(merged[i, ], w9_voc_vars, voc_labels)
}, 1:nrow(merged))

# Select final variables
final_data <- merged %>% select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Output written to data/output/cleaned_data.csv\n')
cat('Final dataset dimensions:', nrow(final_data), 'rows,', ncol(final_data), 'columns\n')
cat('Variables:', paste(names(final_data), collapse = ', '), '\n')
