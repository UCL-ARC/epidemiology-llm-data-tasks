
# Load required packages
library(haven)
library(dplyr)
library(readr)
library(utils)  # For write.csv

# Load datasets with explicit column types
wave_one <- readr::read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave_two <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave_three <- readr::read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave_eight <- readr::read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave_nine_main <- readr::read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Create base dataset
base_data <- wave_one %>% select(NSID)

# Merge wave_two data with explicit column selection
wave_two_data <- wave_two %>% select(NSID, urbind, gor) %>% rename(urbind_w2 = urbind, gor_w2 = gor)
merged_data <- base_data %>%
  left_join(wave_two_data, by = "NSID") %>%
  mutate(regub15 = ifelse(is.na(urbind_w2), -3, urbind_w2),
         regov15 = ifelse(is.na(gor_w2), -3, gor_w2))

# Merge wave_three data with explicit column selection
wave_three_data <- wave_three %>% select(NSID, urbind, gor) %>% rename(urbind_w3 = urbind, gor_w3 = gor)
merged_data <- merged_data %>%
  left_join(wave_three_data, by = "NSID") %>%
  mutate(regub16 = ifelse(is.na(urbind_w3), -3, urbind_w3),
         regov16 = ifelse(is.na(gor_w3), -3, gor_w3))

# Merge wave_eight data
wave_eight_data <- wave_eight %>% select(NSID, W8DGOR)
merged_data <- merged_data %>%
  left_join(wave_eight_data, by = "NSID") %>%
  mutate(regor25 = ifelse(is.na(W8DGOR), -3, W8DGOR))

# Merge wave_nine data
wave_nine_data <- wave_nine_main %>% select(NSID, W9NATIONRES)
merged_data <- merged_data %>%
  left_join(wave_nine_data, by = "NSID") %>%
  mutate(regint32 = ifelse(is.na(W9NATIONRES), -3, W9NATIONRES))

# Define labels
urb_labels <- c('Urban >= 10k - sparse', 'Town & Fringe - sparse', 'Village - sparse',
               'Hamlet and Isolated Dwelling - sparse', 'Urban >= 10k - less sparse',
               'Town & Fringe - less sparse', 'Village - less sparse', 'Hamlet & Isolated Dwelling')

gor_labels <- c('North East', 'North West', 'Yorkshire and the Humber', 'East Midlands',
                'West Midlands', 'East of England', 'London', 'South East', 'South West')

regor25_labels <- c('North East', 'North West', 'Yorkshire and the Humber', 'East Midlands',
                    'West Midlands', 'East of England', 'London', 'South East', 'South West',
                    'Wales', 'Scotland', 'Northern Ireland', 'Unknown due to faulty/missing postcode')

natres_labels <- c('England', 'Scotland', 'Wales', 'Northern Ireland', 'Outside of UK or unknown')

# Create factors with explicit levels
merged_data <- merged_data %>%
  mutate(
    regub15 = factor(regub15, levels = unique(c(1:8, -3)), labels = c(urb_labels, 'Missing')),
    regub16 = factor(regub16, levels = unique(c(1:8, -3)), labels = c(urb_labels, 'Missing')),
    regov15 = factor(regov15, levels = unique(c(1:9, -3)), labels = c(gor_labels, 'Missing')),
    regov16 = factor(regov16, levels = unique(c(1:9, -3)), labels = c(gor_labels, 'Missing')),
    regor25 = factor(regor25, levels = unique(c(1:13, -3)), labels = c(regor25_labels, 'Missing')),
    regint32 = factor(regint32, levels = unique(c(1:5, -3)), labels = c(natres_labels, 'Missing'))
  )

# Select final variables
final_vars <- c('NSID', 'regub15', 'regub16', 'regov15', 'regov16', 'regor25', 'regint32')
final_data <- merged_data %>% select(all_of(final_vars))

# Write to CSV with explicit path
output_path <- 'data/output/cleaned_data.csv'
dir.create(file.path(dirname(output_path)), showWarnings = FALSE, recursive = TRUE)
write.csv(final_data, file = output_path, row.names = FALSE)

# Verify the output
cat('Data successfully written to:', output_path, '\n')
cat('Number of rows:', nrow(final_data), '\n')
cat('Number of columns:', ncol(final_data), '\n')
