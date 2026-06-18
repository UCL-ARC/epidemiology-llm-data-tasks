library(haven)
library(dplyr)
library(readr)
library(labelled)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character()))
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character(), W6SexualityYP = col_double()))
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = cols(NSID = col_character(), W7SexualityYP = col_double()))
ns8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = cols(NSID = col_character(), W8SEXUALITY = col_double()))
ns9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols(NSID = col_character(), W9SORI = col_double()))

# Merge all datasets by NSID
combined <- full_join(wave1, wave4, by = 'NSID')
combined <- full_join(combined, wave6, by = 'NSID')
combined <- full_join(combined, wave7, by = 'NSID')
combined <- full_join(combined, ns8, by = 'NSID')
combined <- full_join(combined, ns9, by = 'NSID')

# Define recoding function for sori variables
recode_sori <- function(x, mapping) {
  x <- factor(x, levels = levels(x))
  x <- x %>% 
    recode(
      as.character(x),
      as.numeric(mapping)
    )
  if (is.factor(x)) {
    x <- as.numeric(x)
  }
  return(x)
}

# Create sori19 from W6SexualityYP (age 19)
# W6SexualityYP value labels: -97=Respondent declined self completion, -92=Refused, -91=Not applicable, -1=Don't know, 1=Heterosexual/Straight, 2=Gay/Lesbian, 3=Bisexual, 4=Other
# Additional requirements: -97 and -100 -> -9, W9SORI value 5 -> -7
# For wave 6, source value 5 doesn't exist, so we only need to handle -97 -> -9
sori19 <- combined$W6SexualityYP
# Map according to additional requirements: -97 and -100 to -9
sori19 <- recode(as.character(sori19),
  "-97" = "-9",
  "-92" = "-9",  # Refused
  "-91" = "-3",  # Not applicable
  "-1" = "-8",   # Don't know
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  .default = "-3"  # Not asked
)
sori19 <- as.integer(sori19)
combined$sori19 <- sori19

# Create sori20 from W7SexualityYP (age 20)
# W7SexualityYP value labels: -100=Respondent declined sexual experience questions, -97=Refused self completion, -92=Refused, -91=Not applicable, -1=Don't know, 1=Heterosexual/Straight, 2=Gay/Lesbian, 3=Bisexual, 4=Other
sori20 <- combined$W7SexualityYP
sori20 <- recode(as.character(sori20),
  "-100" = "-9",  # Additional requirement: -100 -> -9
  "-97" = "-9",   # Additional requirement: -97 -> -9
  "-92" = "-9",   # Refused
  "-91" = "-3",   # Not applicable
  "-1" = "-8",    # Don't know
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  .default = "-3"
)
sori20 <- as.integer(sori20)
combined$sori20 <- sori20

# Create sori25 from W8SEXUALITY (age 25)
# W8SEXUALITY value labels: -9=Refused, -8=Don't know, -1=Not applicable, 1=Heterosexual/Straight, 2=Gay/Lesbian, 3=Bisexual, 4=Other
sori25 <- combined$W8SEXUALITY
sori25 <- recode(as.character(sori25),
  "-9" = "-9",
  "-8" = "-8",
  "-1" = "-1",
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  .default = "-3"
)
sori25 <- as.integer(sori25)
combined$sori25 <- sori25

# Create sori32 from W9SORI (age 32)
# W9SORI value labels: -9=Refused, -8=Don't know, -3=Not asked at fieldwork stage, -1=Not applicable, 1=Heterosexual/Straight, 2=Gay/Lesbian, 3=Bisexual, 4=Other, 5=Prefer not to say
# Additional requirements: -97 and -100 to -9, W9SORI source value 5 to -7
sori32 <- combined$W9SORI
sori32 <- recode(as.character(sori32),
  "-9" = "-9",
  "-8" = "-8",
  "-3" = "-3",
  "-1" = "-1",
  "5" = "-7",   # Additional requirement: 5 (Prefer not to say) -> -7
  "1" = "1",
  "2" = "2",
  "3" = "3",
  "4" = "4",
  .default = "-3"
)
sori32 <- as.integer(sori32)
combined$sori32 <- sori32

# Select final variables
final_vars <- combined %>% select(NSID, sori19, sori20, sori25, sori32)

# Convert sori variables to proper factor with labels
final_vars$sori19 <- factor(final_vars$sori19, levels = c(-9, -7, -3, -8, -1, 1:4), labels = c('Refusal', 'Prefer not to say', 'Not asked', 'Don\'t know', 'Not applicable', 'Heterosexual', 'Gay/lesbian', 'Bisexual', 'Other'))
final_vars$sori20 <- factor(final_vars$sori20, levels = c(-9, -7, -3, -8, -1, 1:4), labels = c('Refusal', 'Prefer not to say', 'Not asked', 'Don\'t know', 'Not applicable', 'Heterosexual', 'Gay/lesbian', 'Bisexual', 'Other'))
final_vars$sori25 <- factor(final_vars$sori25, levels = c(-9, -7, -3, -8, -1, 1:4), labels = c('Refusal', 'Prefer not to say', 'Not asked', 'Don\'t know', 'Not applicable', 'Heterosexual', 'Gay/lesbian', 'Bisexual', 'Other'))
final_vars$sori32 <- factor(final_vars$sori32, levels = c(-9, -7, -3, -8, -1, 1:4), labels = c('Refusal', 'Prefer not to say', 'Not asked', 'Don\'t know', 'Not applicable', 'Heterosexual', 'Gay/lesbian', 'Bisexual', 'Other'))

# Write output
write_csv(final_vars, 'data/output/cleaned_data.csv')

# Print confirmation
cat('Dataset cleaned successfully. Variables: NSID, sori19, sori20, sori25, sori32\n')
cat('Total cases:', nrow(final_vars), '\n')
