
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

# Load and merge all datasets
data_frames <- lapply(files, function(file) {
  read_delim(file, delim = '\t')
})

merged_df <- data_frames[[1]]
for (df in data_frames[-1]) {
  merged_df <- full_join(merged_df, df, by = 'NSID')
}

# Standardize missing values
standardize_missing <- function(x) {
  if (is.numeric(x)) {
    x[x == -99] <- -3
    x[x %in% c(-97, -96, -997, -998, -995, -996)] <- -2
    x[x == -92] <- -9
    x[x == -91] <- -1
    x[x == -8] <- -8
    x[is.na(x)] <- -3
  }
  return(x)
}

# Apply standardization to relevant variables
alc_vars <- c('W1alceverYP', 'W1alcmonYP', 'W2alceverYP', 'W3alceverYP',
              'W4AlcEverYP', 'W6AlcEverYP', 'W7AlcEverYP', 'W8AUDIT1', 'W9AUDIT1')
merged_df[alc_vars] <- lapply(merged_df[alc_vars], standardize_missing)

# Define age mapping
age_mapping <- c(
  W1alceverYP = 14, W1alcmonYP = 14,
  W2alceverYP = 15,
  W3alceverYP = 16,
  W4AlcEverYP = 17,
  W6AlcEverYP = 19,
  W7AlcEverYP = 20,
  W8AUDIT1 = 25,
  W9AUDIT1 = 32
)

# Create alcfst variable using row-wise operation
n <- nrow(merged_df)
alcfst_values <- numeric(n)

for (i in 1:n) {
  row <- merged_df[i, ]

  # Check for drinking indicators with proper handling of NA values
  drinking_indicators <- c(
    W1alceverYP = !is.na(row$W1alceverYP) && row$W1alceverYP == 1,
    W1alcmonYP = !is.na(row$W1alcmonYP) && row$W1alcmonYP == 1,
    W2alceverYP = !is.na(row$W2alceverYP) && row$W2alceverYP == 1,
    W3alceverYP = !is.na(row$W3alceverYP) && row$W3alceverYP == 1,
    W4AlcEverYP = !is.na(row$W4AlcEverYP) && row$W4AlcEverYP == 1,
    W6AlcEverYP = !is.na(row$W6AlcEverYP) && row$W6AlcEverYP == 1,
    W7AlcEverYP = !is.na(row$W7AlcEverYP) && row$W7AlcEverYP == 1,
    W8AUDIT1 = !is.na(row$W8AUDIT1) && row$W8AUDIT1 > 1,
    W9AUDIT1 = !is.na(row$W9AUDIT1) && row$W9AUDIT1 > 1
  )

  # Special rule for age 14
  if (!is.na(row$W1alceverYP) && !is.na(row$W1alcmonYP) &&
      row$W1alceverYP == 1 && row$W1alcmonYP == 1) {
    drinking_indicators['W1alceverYP'] <- TRUE
  }

  # Determine earliest age with drinking
  drinking_vars <- names(drinking_indicators)[drinking_indicators]
  if (length(drinking_vars) > 0) {
    alcfst_values[i] <- min(age_mapping[drinking_vars])
  } else {
    # Check if never had alcohol
    has_missing <- any(is.na(row[alc_vars]))
    if (has_missing) {
      alcfst_values[i] <- -8
    } else {
      alcfst_values[i] <- 99
    }
  }
}

# Add alcfst column to dataframe
merged_df$alcfst <- alcfst_values

# Convert to factor
levels <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
labels <- c('Age 14', 'Age 15', 'Age 16', 'Age 17', 'Age 19', 'Age 20',
            'Age 25', 'Age 32', 'Never had alcohol', 'Don\'t know/insufficient information')
merged_df$alcfst <- factor(merged_df$alcfst, levels = levels, labels = labels)

# Write output
write_csv(merged_df %>% select(NSID, alcfst), 'data/output/cleaned_data.csv')
