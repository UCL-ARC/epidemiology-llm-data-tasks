library(haven)
library(dplyr)
library(readr)

# Load all datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')
wave9_main <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Start with wave1 and merge
data <- wave1
data <- left_join(data, wave2, by = 'NSID')
data <- left_join(data, wave3, by = 'NSID')
data <- left_join(data, wave8, by = 'NSID')
data <- left_join(data, wave9_derived, by = 'NSID')
data <- left_join(data, wave9_main, by = 'NSID')

# Extract urbind and gor from wave2 (age 15)
urbind_15 <- as.integer(data$urbind.x)
gor_15 <- as.integer(data$gor.x)

# Harmonize urbind -94 = insufficient info -> -8
urbind_15 <- ifelse(urbind_15 == -94, -8, urbind_15)
urbind_15 <- ifelse(is.na(urbind_15) | urbind_15 == 0, -1, urbind_15)

# Extract urbind and gor from wave3 (age 16)
urbind_16 <- as.integer(data$urbind.y)
gor_16 <- as.integer(data$gor.y)

# Harmonize urbind
urbind_16 <- ifelse(urbind_16 == -94, -8, urbind_16)
urbind_16 <- ifelse(is.na(urbind_16) | urbind_16 == 0, -1, urbind_16)

# Harmonize gor -94 = insufficient info -> -8
gor_15 <- ifelse(gor_15 == -94, -8, gor_15)
gor_15 <- ifelse(is.na(gor_15) | gor_15 <= 0, -1, gor_15)
gor_16 <- ifelse(gor_16 == -94, -8, gor_16)
gor_16 <- ifelse(is.na(gor_16) | gor_16 <= 0, -1, gor_16)

# Assign to output variables
data$regub15 <- urbind_15
data$regub16 <- urbind_16
data$regov15 <- gor_15
data$regov16 <- gor_16

# Extract W8DGOR (age 25)
gor_25 <- as.integer(data$W8DGOR)
# Valid: 1-13, -9, -8, -1 are missing
gor_25_valid <- ifelse(gor_25 >= 1 & gor_25 <= 13, gor_25, NA)
gor_25_missing <- ifelse(!is.na(gor_25_valid), -1, -1)
gor_25 <- ifelse(is.na(gor_25_valid), -1, -1)
gor_25 <- ifelse(gor_25 >= 1 & gor_25 <= 13, gor_25, -1)
data$regor25 <- as.integer(gor_25)

# Extract W9DRGN (age 32)
gor_32 <- as.integer(data$W9DRGN)
gor_32 <- ifelse(gor_32 >= 1 & gor_32 <= 13, gor_32, -1)
data$regor32 <- as.integer(gor_32)

# Extract W9NATIONRES (age 32)
nation_32 <- as.integer(data$W9NATIONRES)
nation_32 <- ifelse(nation_32 >= 1 & nation_32 <= 5, nation_32, -1)
data$regint32 <- as.integer(nation_32)

# Define factor levels and labels
regub_levels <- c(-1, -8, 1, 2, 3, 4, 5, 6, 7, 8)
regub_labels <- c('Not applicable', "Don't know/Insufficient information",
                  'Urban >= 10k - sparse', 'Town & Fringe - sparse', 'Village - sparse',
                  'Hamlet and Isolated Dwelling - sparse', 'Urban >= 10k - less sparse',
                  'Town & Fringe - less sparse', 'Village - less sparse', 'Hamlet & Isolated Dwelling')

regov_levels <- c(-1, -8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
regov_labels <- c('Not applicable', "Don't know/Insufficient information",
                  'North East', 'North West', 'Yorkshire and the Humber', 'East Midlands',
                  'West Midlands', 'East of England', 'London', 'South East', 'South West',
                  'Wales', 'Scotland', 'Northern Ireland', 'Unknown due to faulty/missing postcode')

regint_levels <- c(-1, -8, -9, 1, 2, 3, 4, 5)
regint_labels <- c('Not applicable', "Don't know/Insufficient information", 'Refused',
                   'England', 'Scotland', 'Wales', 'Northern Ireland', 'Outside of UK or unknown')

# Convert to factors with labels
data$regub15 <- factor(data$regub15, levels = regub_levels, labels = regub_labels)
data$regub16 <- factor(data$regub16, levels = regub_levels, labels = regub_labels)
data$regov15 <- factor(data$regov15, levels = regov_levels, labels = regov_labels)
data$regov16 <- factor(data$regov16, levels = regov_levels, labels = regov_labels)
data$regor25 <- factor(data$regor25, levels = regov_levels, labels = regov_labels)
data$regor32 <- factor(data$regor32, levels = regov_levels, labels = regov_labels)
data$regint32 <- factor(data$regint32, levels = regint_levels, labels = regint_labels)

# Select only required variables
output_vars <- c('NSID', 'regub15', 'regub16', 'regov15', 'regov16', 'regor25', 'regor32', 'regint32')
output_data <- data %>% select(all_of(output_vars))

# Write to CSV
write_csv(output_data, 'data/output/cleaned_data.csv')

cat('Script completed successfully.\n')
cat('Output written to data/output/cleaned_data.csv\n')