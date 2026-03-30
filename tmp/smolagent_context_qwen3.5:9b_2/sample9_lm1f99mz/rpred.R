library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define input files
files <- list(
  wave1 = 'data/input/wave_one_lsype_family_background_2020.tab',
  wave2 = 'data/input/wave_two_lsype_family_background_2020.tab',
  wave4 = 'data/input/wave_four_lsype_family_background_2020.tab'
)

# Load all files
wave1 <- read_delim(files$wave1, delim = '\t', show_col_types = FALSE)
wave2 <- read_delim(files$wave2, delim = '\t', show_col_types = FALSE)
wave4 <- read_delim(files$wave4, delim = '\t', show_col_types = FALSE)

cat('Wave 1 rows:', nrow(wave1), '\n')
cat('Wave 2 rows:', nrow(wave2), '\n')
cat('Wave 4 rows:', nrow(wave4), '\n')

# Create maternal consolidation data - all waves combined
mum_data <- bind_rows(
  wave1 %>% select(NSID, W1hiqualmum) %>% mutate(wave = 1, qual = W1hiqualmum),
  wave2 %>% select(NSID, W2hiqualmum) %>% mutate(wave = 2, qual = W2hiqualmum),
  wave4 %>% select(NSID, w4hiqualmum) %>% mutate(wave = 4, qual = w4hiqualmum)
)

# For each NSID, find the first valid value (positive or negative) in wave order
# Prioritize positive values (1-20), then fall back to negative values
educdtlma <- mum_data %>%
  group_by(NSID) %>%
  arrange(wave) %>%
  mutate(
    is_positive = qual >= 1 & qual <= 20,
    is_negative = qual < 1 & !is.na(qual)
  ) %>%
  # Keep first positive value; if none, keep first negative
  filter(is_positive | (is_negative & row_number() == 1)) %>%
  slice(1) %>%
  ungroup() %>%
  select(NSID, educdtlma = qual)

cat('Consolidated maternal:', nrow(educdtlma), 'rows', '\n')
cat('Unique NSIDs in maternal:', length(unique(educdtlma$NSID)), '\n')

# Create paternal consolidation data
dad_data <- bind_rows(
  wave1 %>% select(NSID, W1hiqualdad) %>% mutate(wave = 1, qual = W1hiqualdad),
  wave2 %>% select(NSID, W2hiqualdad) %>% mutate(wave = 2, qual = W2hiqualdad),
  wave4 %>% select(NSID, w4hiqualdad) %>% mutate(wave = 4, qual = w4hiqualdad)
)

educdtlpa <- dad_data %>%
  group_by(NSID) %>%
  arrange(wave) %>%
  mutate(
    is_positive = qual >= 1 & qual <= 20,
    is_negative = qual < 1 & !is.na(qual)
  ) %>%
  filter(is_positive | (is_negative & row_number() == 1)) %>%
  slice(1) %>%
  ungroup() %>%
  select(NSID, educdtlpa = qual)

cat('Consolidated paternal:', nrow(educdtlpa), 'rows', '\n')
cat('Unique NSIDs in paternal:', length(unique(educdtlpa$NSID)), '\n')

# Get all unique NSIDs from wave1
all_nsid <- wave1$NSID

# Start with base dataset
final_base <- as.data.frame(wave1[, c('NSID')], stringsAsFactors = FALSE)

# Merge maternal education (left join)
merged_mum <- merge(final_base, educdtlma, by = 'NSID', all.x = TRUE)

cat('After maternal merge:', nrow(merged_mum), 'rows', '\n')

# Merge paternal education (left join)
merged_final <- merge(merged_mum, educdtlpa, by = 'NSID', all.x = TRUE)

cat('After paternal merge:', nrow(merged_final), 'rows', '\n')

# Function to map detailed to collapsed values
map_educ <- function(x) {
  ifelse(x %in% c(1:4), 0L, 
    ifelse(x %in% c(5:17), 1L,
      ifelse(x %in% 18, 2L,
        ifelse(x %in% 19, 3L,
          ifelse(x %in% 20, 4L,
            NA_real_
          )
        )
      )
    )
  )
}

# Create collapsed maternal education
merged_final$educma <- map_educ(merged_final$educdtlma)
merged_final$educma <- factor(merged_final$educma, 
  levels = c(-999, -99, -98, -94, -92, -91, -1, 0, 1, 2, 3, 4),
  labels = c('Missing - household data lost',
            'Missing - mother not interviewed',
            'Missing - mother not present',
            'Missing - insufficient information',
            'Missing - refused',
            'Missing - not applicable',
            'Missing - not asked',
            'NVQ 4-5: degree-level',
            'NVQ 1-3: sub-degree',
            'None/entry: training below NVQ',
            'Other: level unspecified',
            'No qualifications mentioned')
)

# Create collapsed paternal education
merged_final$educpa <- map_educ(merged_final$educdtlpa)
merged_final$educpa <- factor(merged_final$educpa, 
  levels = c(-999, -99, -98, -94, -92, -91, -1, 0, 1, 2, 3, 4),
  labels = c('Missing - household data lost',
            'Missing - father not interviewed',
            'Missing - father not present',
            'Missing - insufficient information',
            'Missing - refused',
            'Missing - not applicable',
            'Missing - not asked',
            'NVQ 4-5: degree-level',
            'NVQ 1-3: sub-degree',
            'None/entry: training below NVQ',
            'Other: level unspecified',
            'No qualifications mentioned')
)

# Build final dataset with exactly 5 variables
final_output <- merged_final %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

cat('Output variables:', paste(names(final_output), collapse = ', '), '\n')
cat('Output rows:', nrow(final_output), '\n')
cat('Output columns:', ncol(final_output), '\n')

# Write output
write_csv(final_output, 'data/output/cleaned_data.csv')

cat('Script completed successfully!\n')
cat('Output written to data/output/cleaned_data.csv\n')
