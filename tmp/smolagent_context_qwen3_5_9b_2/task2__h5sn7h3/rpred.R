library(haven)
library(dplyr)
library(readr)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
n8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
n9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Standard missing value mapping function
standard_miss <- function(x) {
  x[x %in% c(-999, -998, -997, -995)] <- -2
  x[x == -99] <- -3
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x
}

# Apply standard missing codes
w1$W1ethnic2YP <- standard_miss(w1$W1ethnic2YP)
w2$W2ethnicYP <- standard_miss(w2$W2ethnicYP)
w4$w4ethnic2YP <- standard_miss(w4$w4ethnic2YP)
n8$W8DETHN15 <- standard_miss(n8$W8DETHN15)
n9$W9DETHN15 <- standard_miss(n9$W9DETHN15)

# Merge all datasets by NSID
combined <- full_join(w1, w2, by = 'NSID')
combined <- full_join(combined, w4, by = 'NSID')
combined <- full_join(combined, n8, by = 'NSID')
combined <- full_join(combined, n9, by = 'NSID')

# Derive eth variable: earliest valid positive response first
result <- combined %>%
  mutate(
    eth = case_when(
      !is.na(W1ethnic2YP) & W1ethnic2YP >= 1 ~ W1ethnic2YP,
      !is.na(W2ethnicYP) & W2ethnicYP >= 1 ~ W2ethnicYP,
      !is.na(w4ethnic2YP) & w4ethnic2YP >= 1 ~ w4ethnic2YP,
      !is.na(W8DETHN15) & W8DETHN15 >= 1 ~ W8DETHN15,
      !is.na(W9DETHN15) & W9DETHN15 >= 1 ~ W9DETHN15,
      TRUE ~ as.integer(NA)
    )
  ) %>%
  select(NSID, eth)

# Check unique values
cat('Unique eth values:', sort(unique(result$eth)), '\n')
cat('Eth counts:', table(result$eth), '\n')

# Create labels for eth - using proper format
label_values <- c(
  '1' = 'White - British',
  '2' = 'White - Irish',
  '3' = 'Any other White background',
  '4' = 'Mixed - White and Black Caribbean',
  '5' = 'Mixed - White and Black African',
  '6' = 'Mixed - White and Asian',
  '7' = 'Any other mixed background',
  '8' = 'Indian',
  '9' = 'Pakistani',
  '10' = 'Bangladeshi',
  '11' = 'Any other Asian background',
  '12' = 'Black Caribbean',
  '13' = 'Black African',
  '14' = 'Any other Black background',
  '15' = 'Chinese',
  '16' = 'Any other ethnic background',
  '-9' = 'Refused',
  '-8' = 'Insufficient information',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked',
  '-2' = 'Schedule not applicable / script error / information lost',
  '-1' = 'Not applicable'
)

# Convert to factor with proper labels
result$eth <- factor(result$eth, 
                     levels = c(1:16, -9, -8, -7, -3, -2, -1),
                     labels = c('White - British', 'White - Irish', 'Any other White background', 
                               'Mixed - White and Black Caribbean', 'Mixed - White and Black African', 
                               'Mixed - White and Asian', 'Any other mixed background', 
                               'Indian', 'Pakistani', 'Bangladeshi', 
                               'Any other Asian background', 'Black Caribbean', 
                               'Black African', 'Any other Black background', 'Chinese', 
                               'Any other ethnic background', 'Refused', 
                               'Insufficient information', 'Prefer not to say', 
                               'Not asked', 'Schedule not applicable / script error / information lost', 
                               'Not applicable'))

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

# Print summary
cat('Final rows:', nrow(result), '\n')
cat('Final variables:', names(result), '\n')