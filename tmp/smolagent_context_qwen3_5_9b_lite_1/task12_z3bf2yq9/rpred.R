library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = "\t")
ns9_2022 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = "\t")
ns8_2015 <- read_delim('data/input/ns8_2015_derived.tab', delim = "\t")
wave_six <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = "\t")
wave_seven <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = "\t")
wave_five <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = "\t")
wave_four <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = "\t")

# Function to collapse NS-SEC to 13 major categories
collapse_to_major <- function(x) {
  x_clean <- ifelse(is.na(x) | x %in% c(-1, -2, -3, -8, -9, -91, -99, -999), NA_real_, x)
  x_clean[x_clean %in% c(1, 2)] <- 1
  x_clean[x_clean %in% c(3, 3.1, 3.2, 3.3, 3.4, 4, 4.1, 4.2, 4.3, 4.4, 5)] <- 6
  x_clean[x_clean %in% c(6, 7, 7.1, 7.2, 7.3, 7.4, 8, 8.1, 8.2)] <- 6
  x_clean[x_clean == 9] <- 9
  x_clean[x_clean == 10] <- 10
  x_clean[x_clean %in% c(11, 11.1, 11.2)] <- 11
  x_clean[x_clean %in% c(12, 12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7)] <- 12
  x_clean[x_clean %in% c(13, 13.1, 13.2, 13.3, 13.4, 13.5)] <- 13
  x_clean[x_clean == 14] <- 14
  x_clean[x_clean == 15] <- 15
  return(x_clean)
}

# Process wave4 (age 17)
nssec17 <- wave_four %>%
  select(NSID, W4nsseccatYP) %>%
  mutate(nssec17 = collapse_to_major(W4nsseccatYP))

# Process wave5 (age 18)
nssec18 <- wave_five %>%
  select(NSID, W5nsseccatYP) %>%
  mutate(nssec18 = collapse_to_major(W5nsseccatYP))

# Process wave6 (age 19)
nssec19 <- wave_six %>%
  select(NSID, w6nsseccatYP) %>%
  mutate(nssec19 = collapse_to_major(w6nsseccatYP))

# Process wave7 (age 20)
nssec20 <- wave_seven %>%
  select(NSID, W7NSSECCat) %>%
  rename(nssec20 = W7NSSECCat) %>%
  mutate(nssec20 = collapse_to_major(nssec20))

# Process wave8 (age 25)
nssec25 <- ns8_2015 %>%
  select(NSID, W8DNSSEC17) %>%
  rename(nssec25 = W8DNSSEC17) %>%
  mutate(nssec25 = collapse_to_major(nssec25))

# Process wave9 (age 32)
nssec32 <- ns9_2022 %>%
  select(NSID, W9NSSEC) %>%
  rename(nssec32 = W9NSSEC) %>%
  mutate(nssec32 = collapse_to_major(nssec32))

# Full join all datasets
final_data <- wave_one %>%
  select(NSID) %>%
  full_join(nssec17, by = 'NSID') %>%
  full_join(nssec18, by = 'NSID') %>%
  full_join(nssec19, by = 'NSID') %>%
  full_join(nssec20, by = 'NSID') %>%
  full_join(nssec25, by = 'NSID') %>%
  full_join(nssec32, by = 'NSID')

# Remove raw source variables - keep only NSID and nssec variables
final_data <- final_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write output
write_csv(final_data, 'data/output/cleaned_data.csv')

print('Script completed successfully')
print(paste('Total rows:', nrow(final_data)))
print(paste('Total columns:', ncol(final_data)))
print(names(final_data))
print(table(final_data$nssec17, useNA = "always"))