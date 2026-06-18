library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
# Wave 1 - Age 14 (Young Person only has NSID)
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)

# Wave 4 - Age 17 (Young Person only has NSID)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)

# Wave 2 - Age 15 (Family Background has urbind and gor)
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Wave 3 - Age 16 (Family Background has urbind and gor)
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Wave 8 - Age 25 (Derived Variables has W8DGOR)
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)

# Wave 9 - Age 32 (Derived Variables has W9DRGN)
wave9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

# Wave 9 - Age 32 (Main Interview has W9NATIONRES)
wave9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

cat('Files loaded successfully\n')
cat('Wave 1:', nrow(wave1), 'cases\n')
cat('Wave 2:', nrow(wave2), 'cases\n')
cat('Wave 3:', nrow(wave3), 'cases\n')
cat('Wave 4:', nrow(wave4), 'cases\n')
cat('Wave 8:', nrow(wave8), 'cases\n')
cat('Wave 9 Derived:', nrow(wave9_derived), 'cases\n')
cat('Wave 9 Main:', nrow(wave9_main), 'cases\n')

# Merge all files by NSID
full_data <- wave1 %>%
  full_join(wave2, by = 'NSID') %>%
  full_join(wave3, by = 'NSID') %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9_derived, by = 'NSID') %>%
  full_join(wave9_main, by = 'NSID')

cat('Merged data:', nrow(full_data), 'cases, ', ncol(full_data), 'columns\n')

# Function to convert missing values to standard codes
convert_missing <- function(x) {
  # Map various missing codes to standard scheme based on label meaning
  x[x == -999] <- -2  # Schedule not applicable / information lost
  x[x == -998] <- -2  # Schedule not applicable / script error
  x[x == -997] <- -2  # Schedule not applicable
  x[x == -995] <- -2  # Schedule not applicable
  x[x == -94] <- -8   # Insufficient information / Don't know
  x[x == -92] <- -9   # Refusal (based on common pattern)
  x[x == -91] <- -1   # Not applicable (based on common pattern)
  x[x == -99] <- -3   # Not asked at fieldwork
  x[x == -100] <- -2  # Depends on label, default to not applicable
  x[x == -97] <- -2   # Depends on label, default to not applicable
  
  # Convert remaining NAs to -3 (not asked at fieldwork)
  x[is.na(x)] <- -3
  
  return(x)
}

# Process wave 2 (age 15) - urbind (regub15) and gor (regov15)
# After merge: urbind.x from wave2, gor.x from wave2
full_data$regub15 <- convert_missing(full_data$urbind.x)
full_data$regov15 <- convert_missing(full_data$gor.x)

# Process wave 3 (age 16) - urbind (regub16) and gor (regov16)
# After merge: urbind.y from wave3, gor.y from wave3
full_data$regub16 <- convert_missing(full_data$urbind.y)
full_data$regov16 <- convert_missing(full_data$gor.y)

# Process wave 8 (age 25) - W8DGOR (regor25)
full_data$regor25 <- convert_missing(full_data$W8DGOR)

# Process wave 9 (age 32) - W9DRGN (regor32)
full_data$regor32 <- convert_missing(full_data$W9DRGN)

# Process wave 9 (age 32) - W9NATIONRES (regint32)
full_data$regint32 <- convert_missing(full_data$W9NATIONRES)

cat('Derived variables created\n')

# Check for any remaining NAs
cat('Remaining NAs in derived variables:\n')
cat('regub15:', sum(is.na(full_data$regub15)), '\n')
cat('regov15:', sum(is.na(full_data$regov15)), '\n')
cat('regub16:', sum(is.na(full_data$regub16)), '\n')
cat('regov16:', sum(is.na(full_data$regov16)), '\n')
cat('regor25:', sum(is.na(full_data$regor25)), '\n')
cat('regor32:', sum(is.na(full_data$regor32)), '\n')
cat('regint32:', sum(is.na(full_data$regint32)), '\n')

# Keep only final variables
final_data <- full_data %>%
  select(NSID, regub15, regov15, regub16, regov16, regor25, regor32, regint32)

cat('Final data dimensions:', nrow(final_data), 'x', ncol(final_data), '\n')

# Write to output
write_csv(final_data, 'data/output/cleaned_data.csv')
cat('Output written to data/output/cleaned_data.csv\n')

# Show sample of output
head(final_data)
