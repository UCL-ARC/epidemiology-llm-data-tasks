library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Read all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Full join all waves on NSID
cohort <- full_join(wave1, wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID')

# Function to map missing values to standard codes based on label meaning
map_missing_W1 <- function(x) {
  result <- x
  result[is.na(result)] <- -3  # Not interviewed
  result[result == -99] <- -3  # Not interviewed
  result[result == -999] <- -3  # Not interviewed
  result[result == -92] <- -9  # Refused
  result[result == -91] <- -1  # Not applicable
  result[result == -1] <- -8   # Don't know
  return(result)
}

map_missing_W2 <- function(x) {
  result <- x
  result[is.na(result)] <- -3  # Not interviewed
  result[result == -998] <- -2  # Schedule not applicable / information lost
  result[result == -997] <- -2  # Script error
  result[result == -995] <- -2  # Missing history section data
  result[result == -99] <- -3  # Not interviewed
  result[result == -92] <- -9  # Refused
  result[result == -91] <- -1  # Not applicable
  result[result == -1] <- -8   # Don't know
  return(result)
}

# Create standardised versions
cohort <- cohort %>%
  mutate(
    W1_std = map_missing_W1(W1englangYP),
    W2_std = map_missing_W2(W2EnglangYP)
  )

# Create consolidated 'lang' variable using earliest-valid-first approach
cohort <- cohort %>%
  mutate(
    lang = case_when(
      !is.na(W2_std) & W2_std > -1 ~ W2_std,
      !is.na(W1_std) & W1_std > -1 ~ W1_std,
      TRUE ~ -3
    )
  )

# Select only ID and the final lang variable
final_data <- cohort %>%
  select(NSID, lang)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print summary
cat('Data cleaned successfully. Rows:', nrow(final_data), '\n')
cat('Variable lang distribution:\n')
table(final_data$lang)
'}]` is the tool call. I need to fix the script without the labels first to see if it works. If it works, I can add labels later or check if labels are needed. Let me simplify and just write the cleaned data. Looking at the instructions again, I need to create a labelled factor with explicit labels. Let me try a different approach using labelled::make_labelled(). Actually, looking at it again, I should just use the labelled approach correctly. Let me try using factor with labels directly. I'll use `labelled::make_labelled()` to create a labelled factor. Let me rewrite the script. Actually, let me just not create labels for now and see if the script runs first. If the basic data cleaning works, the task might just need the cleaned CSV without factoring. Let me check the instructions again... The instructions say 