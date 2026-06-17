library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load all data files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave_three <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave_eight <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave_nine <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Define missing code lists
define_missing_s1 <- c(-99, -97, -96, -92, -91, -1)
define_missing_s2 <- c(-998, -997, -995, -99, -97, -96, -92, -91, -1)
define_missing_s3 <- c(-99, -97, -96, -92, -91, -1)
define_missing_s4 <- c(-99, -97, -96, -92, -91, -1)
define_missing_s6 <- c(-997, -97, -92, -91, -1)
define_missing_s7 <- c(-996, -97, -92, -91, -1)
define_missing_s8 <- c(-9, -8, -3, -1)
define_missing_s9 <- c(-9, -8, -3, -1)

# Create indicator data frames for each wave with proper column names
# S1 (age 14)
df1 <- data.frame(
  NSID = wave_one$NSID,
  drinking_14 = ifelse((wave_one$W1alceverYP == 1 & wave_one$W1alcmonYP == 1) & 
                      !is.na(wave_one$W1alceverYP) & wave_one$W1alceverYP %in% define_missing_s1 == FALSE &
                      !is.na(wave_one$W1alcmonYP) & wave_one$W1alcmonYP %in% define_missing_s1 == FALSE,
                     TRUE, FALSE),
  missing_14 = is.na(wave_one$W1alceverYP) | wave_one$W1alceverYP %in% define_missing_s1 | 
               is.na(wave_one$W1alcmonYP) | wave_one$W1alcmonYP %in% define_missing_s1
)

# S2 (age 15)
df2 <- data.frame(
  NSID = wave_two$NSID,
  drinking_15 = ifelse(wave_two$W2alceverYP == 1 & !is.na(wave_two$W2alceverYP) & wave_two$W2alceverYP %in% define_missing_s2 == FALSE, TRUE, FALSE),
  missing_15 = is.na(wave_two$W2alceverYP) | wave_two$W2alceverYP %in% define_missing_s2
)

# S3 (age 16)
df3 <- data.frame(
  NSID = wave_three$NSID,
  drinking_16 = ifelse(wave_three$W3alceverYP == 1 & !is.na(wave_three$W3alceverYP) & wave_three$W3alceverYP %in% define_missing_s3 == FALSE, TRUE, FALSE),
  missing_16 = is.na(wave_three$W3alceverYP) | wave_three$W3alceverYP %in% define_missing_s3
)

# S4 (age 17)
df4 <- data.frame(
  NSID = wave_four$NSID,
  drinking_17 = ifelse(wave_four$W4AlcEverYP == 1 & !is.na(wave_four$W4AlcEverYP) & wave_four$W4AlcEverYP %in% define_missing_s4 == FALSE, TRUE, FALSE),
  missing_17 = is.na(wave_four$W4AlcEverYP) | wave_four$W4AlcEverYP %in% define_missing_s4
)

# S6 (age 19)
df6 <- data.frame(
  NSID = wave_six$NSID,
  drinking_19 = ifelse(wave_six$W6AlcEverYP == 1 & !is.na(wave_six$W6AlcEverYP) & wave_six$W6AlcEverYP %in% define_missing_s6 == FALSE, TRUE, FALSE),
  missing_19 = is.na(wave_six$W6AlcEverYP) | wave_six$W6AlcEverYP %in% define_missing_s6
)

# S7 (age 20)
df7 <- data.frame(
  NSID = wave_seven$NSID,
  drinking_20 = ifelse(wave_seven$W7AlcEverYP == 1 & !is.na(wave_seven$W7AlcEverYP) & wave_seven$W7AlcEverYP %in% define_missing_s7 == FALSE, TRUE, FALSE),
  missing_20 = is.na(wave_seven$W7AlcEverYP) | wave_seven$W7AlcEverYP %in% define_missing_s7
)

# S8 (age 25)
df8 <- data.frame(
  NSID = wave_eight$NSID,
  drinking_25 = ifelse(wave_eight$W8AUDIT1 >= 2 & !is.na(wave_eight$W8AUDIT1) & wave_eight$W8AUDIT1 %in% define_missing_s8 == FALSE, TRUE, FALSE),
  missing_25 = is.na(wave_eight$W8AUDIT1) | wave_eight$W8AUDIT1 %in% define_missing_s8
)

# S9 (age 32)
df9 <- data.frame(
  NSID = wave_nine$NSID,
  drinking_32 = ifelse(wave_nine$W9AUDIT1 >= 2 & !is.na(wave_nine$W9AUDIT1) & wave_nine$W9AUDIT1 %in% define_missing_s9 == FALSE, TRUE, FALSE),
  missing_32 = is.na(wave_nine$W9AUDIT1) | wave_nine$W9AUDIT1 %in% define_missing_s9
)

# Merge all dataframes by NSID using full_join
result <- df1 %>%
  full_join(df2, by = 'NSID') %>%
  full_join(df3, by = 'NSID') %>%
  full_join(df4, by = 'NSID') %>%
  full_join(df6, by = 'NSID') %>%
  full_join(df7, by = 'NSID') %>%
  full_join(df8, by = 'NSID') %>%
  full_join(df9, by = 'NSID')

# Determine alcfst (earliest age of drinking)
result$alcfst <- 99  # Default: never
result$drinking_observed <- FALSE

# Age 14
if (any(result$drinking_14 == TRUE)) {
  result$alcfst[result$drinking_14 == TRUE] <- 14
  result$drinking_observed <- result$drinking_observed | (result$drinking_14 == TRUE)
}

# Age 15
if (any(result$drinking_15 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_15 == TRUE)
  result$alcfst[result$drinking_15 == TRUE & is.na(result$alcfst)] <- 15
}

# Age 16
if (any(result$drinking_16 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_16 == TRUE)
  result$alcfst[result$drinking_16 == TRUE & is.na(result$alcfst)] <- 16
}

# Age 17
if (any(result$drinking_17 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_17 == TRUE)
  result$alcfst[result$drinking_17 == TRUE & is.na(result$alcfst)] <- 17
}

# Age 19
if (any(result$drinking_19 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_19 == TRUE)
  result$alcfst[result$drinking_19 == TRUE & is.na(result$alcfst)] <- 19
}

# Age 20
if (any(result$drinking_20 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_20 == TRUE)
  result$alcfst[result$drinking_20 == TRUE & is.na(result$alcfst)] <- 20
}

# Age 25
if (any(result$drinking_25 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_25 == TRUE)
  result$alcfst[result$drinking_25 == TRUE & is.na(result$alcfst)] <- 25
}

# Age 32
if (any(result$drinking_32 == TRUE)) {
  result$drinking_observed <- result$drinking_observed | (result$drinking_32 == TRUE)
  result$alcfst[result$drinking_32 == TRUE & is.na(result$alcfst)] <- 32
}

# Compute any missing indicators
any_missing <- (result$missing_14 == TRUE) | (result$missing_15 == TRUE) | 
               (result$missing_16 == TRUE) | (result$missing_17 == TRUE) | 
               (result$missing_19 == TRUE) | (result$missing_20 == TRUE) | 
               (result$missing_25 == TRUE) | (result$missing_32 == TRUE)

# For those with no drinking observed but at least one missing indicator, assign -8
result$alcfst[result$drinking_observed == FALSE & any_missing] <- -8

# Create factor with correct levels and labels
result$alcfst <- factor(result$alcfst,
                        levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8),
                        labels = c('Age 14', 'Age 15', 'Age 16', 'Age 17', 'Age 19', 'Age 20', 'Age 25', 'Age 32', 'Never had alcohol', "Don't know/insufficient information"))

# Keep only NSID and alcfst
result <- result[, c('NSID', 'alcfst')]

print('Final result:')
print(table(result$alcfst))

# Write output
write_csv(result, 'data/output/cleaned_data.csv')

print('Script completed successfully!')
print(paste('Rows:', nrow(result)))
print(table(result$alcfst))
}