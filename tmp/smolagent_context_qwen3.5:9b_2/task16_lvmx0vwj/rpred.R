library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Set working directory
setwd('/home/jovyan/epidemiology-llm-data-tasks/tmp/smolagent_context/sample16_lvmx0vwj/data')

# Read files
file1 <- read_delim('input/wave_one_lsype_family_background_2020.tab', delim = '\t')
file2 <- read_delim('input/wave_two_lsype_family_background_2020.tab', delim = '\t')
file3 <- read_delim('input/wave_three_lsype_family_background_2020.tab', delim = '\t')
file4 <- read_delim('input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Convert NSID to character for proper joining
file1 <- file1 %>% mutate(NSID = as.character(NSID))
file2 <- file2 %>% mutate(NSID = as.character(NSID))
file3 <- file3 %>% mutate(NSID = as.character(NSID))
file4 <- file4 %>% mutate(NSID = as.character(NSID))

# Rename income variables
file1 <- file1 %>% rename(W1GrsswkHH = W1GrsswkHH)
file2 <- file2 %>% rename(W2GrsswkHH = W2GrsswkHH)
file3 <- file3 %>% rename(W3incestw = W3incestw)
file4 <- file4 %>% rename(w4IncEstW = w4IncEstW)

# Merge all files
merged <- file1 %>%
  full_join(file2, by = 'NSID') %>%
  full_join(file3, by = 'NSID') %>%
  full_join(file4, by = 'NSID')

# Function to map missing codes
map_missing_codes <- function(x, mapping) {
  result <- x
  for (old_code in names(mapping)) {
    result <- ifelse(result == as.numeric(old_code), mapping[[old_code]], result)
  }
  # Handle NA values
  result <- ifelse(is.na(result), -3, result)
  return(as.numeric(result))
}

# Map missing codes for age 14 (W1GrsswkHH)
merged <- merged %>%
  mutate(
    incwhh14_raw = map_missing_codes(W1GrsswkHH, 
      c('-999.0' = -9, '-992.0' = -8, '-99.0' = -3, '-94.0' = -8, 
        '-92.0' = -9, '-91.0' = -1, '-3.0' = -1, '-1.0' = -8))
  )

# Map missing codes for age 15 (W2GrsswkHH)
merged <- merged %>%
  mutate(
    incwhh15_raw = map_missing_codes(W2GrsswkHH, 
      c('-999.0' = -9, '-992.0' = -8, '-99.0' = -3, '-94.0' = -8, 
        '-92.0' = -9, '-91.0' = -1, '-3.0' = -1, '-1.0' = -8))
  )

# Map missing codes for age 16 (W3incestw)
merged <- merged %>%
  mutate(
    incwhh16_raw = map_missing_codes(W3incestw, 
      c('-99.0' = -3, '-92.0' = -9, '-1.0' = -8))
  )

# Map missing codes for age 17 (w4IncEstW)
merged <- merged %>%
  mutate(
    incwhh17_raw = map_missing_codes(w4IncEstW, 
      c('-996.0' = -3, '-99.0' = -3, '-92.0' = -9, '-1.0' = -8))
  )

# Define all possible levels and labels
all_levels <- sort(unique(c(-9, -8, -3, -2, -1, 1:12)))
all_labels <- c(
  '-9' = 'Refusal',
  '-8' = 'Don\'t know/insufficient information',
  '-3' = 'Not asked/interviewed',
  '-2' = 'Script error/lost',
  '-1' = 'Item not applicable',
  '1' = 'Up to \u00a349',
  '2' = '\u00a350 up to \u00a399',
  '3' = '\u00a3100 up to \u00a3199',
  '4' = '\u00a3200 up to \u00a3299',
  '5' = '\u00a3300 up to \u00a3399',
  '6' = '\u00a3400 up to \u00a3499',
  '7' = '\u00a3500 up to \u00a3599',
  '8' = '\u00a3600 up to \u00a3699',
  '9' = '\u00a3700 up to \u00a3799',
  '10' = '\u00a3800 up to \u00a3899',
  '11' = '\u00a3900 up to \u00a3999',
  '12' = '\u00a31,000 or more'
)

# Create banded variables for age 14 and 15
merged <- merged %>%
  mutate(
    incwhh14 = factor(incwhh14_raw, levels = all_levels, labels = all_labels),
    incwhh15 = factor(incwhh15_raw, levels = all_levels, labels = all_labels)
  )

# Create banded variables for age 16 and 17
merged <- merged %>%
  mutate(
    incwhh16 = factor(incwhh16_raw, levels = all_levels, labels = all_labels),
    incwhh17 = factor(incwhh17_raw, levels = all_levels, labels = all_labels)
  )

# Create continuous versions for ages 14-15
merged <- merged %>%
  mutate(
    incwhhcnt14 = incwhh14_raw,
    incwhhcnt15 = incwhh15_raw
  )

# Select only required variables
final_data <- merged %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Write output
write_csv(final_data, 'output/cleaned_data.csv')

cat('Dataset successfully created with', nrow(final_data), 'rows\n')
cat('Variables:', paste(names(final_data), collapse = ', '), '\n')

# Show summary of missing values
for (var in c('incwhh14', 'incwhh15', 'incwhh16', 'incwhh17')) {
  cat('\n', var, ':\n')
  print(table(final_data[[var]]))
}
