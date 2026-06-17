library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all data files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = "\t", show_col_types = FALSE)
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = "\t", show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = "\t", show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = "\t", show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = "\t", show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(wave1, wave4, by = 'NSID')
df <- full_join(df, wave6, by = 'NSID')
df <- full_join(df, wave7, by = 'NSID')
df <- full_join(df, wave8, by = 'NSID')
df <- full_join(df, wave9, by = 'NSID')

# Function to recode missing values to standard scheme
recode_sexuality <- function(x) {
  x[x == -97] <- -2  # Respondent declined self completion -> schedule not applicable
  x[x == -100] <- -2  # Respondent declined sexual experience questions -> schedule not applicable
  x[x == -92] <- -9   # Refused -> Refusal
  x[x == -91] <- -1   # Not applicable -> Not applicable
  x[x == -1] <- -8    # Don't know -> Don't know
  x[x == 5] <- -7     # Prefer not to say -> Prefer not to say
  x[is.na(x)] <- -3   # Not asked at fieldwork stage -> Not asked
  x
}

# Recode W6SexualityYP (Age 19) -> sori19
sori19 <- recode_sexuality(wave6$W6SexualityYP)

# Recode W7SexualityYP (Age 20) -> sori20
sori20 <- recode_sexuality(wave7$W7SexualityYP)

# Recode W8SEXUALITY (Age 25) -> sori25
sori25 <- recode_sexuality(wave8$W8SEXUALITY)

# Recode W9SORI (Age 32) -> sori32
sori32 <- recode_sexuality(wave9$W9SORI)

# Create named vectors for joining
sori19_vec <- set_names(sori19, wave6$NSID)
sori20_vec <- set_names(sori20, wave7$NSID)
sori25_vec <- set_names(sori25, wave8$NSID)
sori32_vec <- set_names(sori32, wave9$NSID)

# Map to the merged dataframe by NSID
df <- df %>%
  mutate(
    sori19 = sori19_vec[NSID],
    sori20 = sori20_vec[NSID],
    sori25 = sori25_vec[NSID],
    sori32 = sori32_vec[NSID]
  )

# Create final output with only NSID and sori variables
output <- df %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')
cat('Output written to data/output/cleaned_data.csv\n')
cat('Number of rows:', nrow(output), '\n')
cat('Variables:', names(output), '\n')
