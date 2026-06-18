library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', col_types = cols(.default = 'c'))
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', col_types = cols(.default = 'c'))
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols(.default = 'c'))

# Merge all datasets by NSID
df <- full_join(wave1, wave2, by = 'NSID')
df <- full_join(df, wave3, by = 'NSID')
df <- full_join(df, wave4, by = 'NSID')
df <- full_join(df, wave6, by = 'NSID')
df <- full_join(df, wave7, by = 'NSID')
df <- full_join(df, wave8, by = 'NSID')
df <- full_join(df, wave9, by = 'NSID')

# Create alcohol ever flags for each wave
# 1 = has ever had alcohol, 2 = never had alcohol
# Missing values: anything <= -1 or NA

# Wave 1 (Age 14)
df$alcever_14 <- ifelse(df$W1alceverYP == 1, 14, NA)
df$alcever_14[is.na(df$W1alceverYP) | df$W1alceverYP <= -1 | df$W1alceverYP == 2] <- NA

# Wave 2 (Age 15)
df$alcever_15 <- ifelse(df$W2alceverYP == 1, 15, NA)
df$alcever_15[is.na(df$W2alceverYP) | df$W2alceverYP <= -1 | df$W2alceverYP == 2] <- NA

# Wave 3 (Age 16)
df$alcever_16 <- ifelse(df$W3alceverYP == 1, 16, NA)
df$alcever_16[is.na(df$W3alceverYP) | df$W3alceverYP <= -1 | df$W3alceverYP == 2] <- NA

# Wave 4 (Age 17)
df$alcever_17 <- ifelse(df$W4AlcEverYP == 1, 17, NA)
df$alcever_17[is.na(df$W4AlcEverYP) | df$W4AlcEverYP <= -1 | df$W4AlcEverYP == 2] <- NA

# Wave 6 (Age 19)
df$alcever_19 <- ifelse(df$W6AlcEverYP == 1, 19, NA)
df$alcever_19[is.na(df$W6AlcEverYP) | df$W6AlcEverYP <= -1 | df$W6AlcEverYP == 2] <- NA

# Wave 7 (Age 20)
df$alcever_20 <- ifelse(df$W7AlcEverYP == 1, 20, NA)
df$alcever_20[is.na(df$W7AlcEverYP) | df$W7AlcEverYP <= -1 | df$W7AlcEverYP == 2] <- NA

# Wave 8 (Age 25): AUDIT1 - 1 = Never, >1 = has drunk
df$alcever_25 <- ifelse(df$W8AUDIT1 > 1, 25, NA)
df$alcever_25[is.na(df$W8AUDIT1) | df$W8AUDIT1 <= -1 | df$W8AUDIT1 == 1] <- NA

# Wave 9 (Age 32): AUDIT1 - 1 = Never, >1 = has drunk
df$alcever_32 <- ifelse(df$W9AUDIT1 > 1, 32, NA)
df$alcever_32[is.na(df$W9AUDIT1) | df$W9AUDIT1 <= -1 | df$W9AUDIT1 == 1] <- NA

# Find earliest age at which they ever had alcohol
alcfst <- pmin(
df$alcever_14, df$alcever_15, df$alcever_16, df$alcever_17,
df$alcever_19, df$alcever_20, df$alcever_25, df$alcever_32,
na.rm = TRUE
)

# If never drank (all NA or >32), set to 99
df$alcfst <- ifelse(is.na(alcfst) | alcfst > 32, 99, as.integer(alcfst))

# Create final output: NSID and alcfst only
output <- df %>% select(NSID, alcfst)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Output rows:', nrow(output)))
print(table(df$alcfst))