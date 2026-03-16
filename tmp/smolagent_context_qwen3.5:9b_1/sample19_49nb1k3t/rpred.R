library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

input_dir <- 'data/input/'
output_dir <- './data/output/'

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

wave1 <- read_delim(paste0(input_dir, 'wave_one_lsype_young_person_2020.tab'), delim = '\t')
wave4 <- read_delim(paste0(input_dir, 'wave_four_lsype_young_person_2020.tab'), delim = '\t')
wave8 <- read_delim(paste0(input_dir, 'ns8_2015_derived.tab'), delim = '\t')
wave9 <- read_delim(paste0(input_dir, 'ns9_2022_derived_variables.tab'), delim = '\t')

merged <- wave1 %>%
  full_join(wave4, by = 'NSID') %>%
  full_join(wave8, by = 'NSID') %>%
  full_join(wave9, by = 'NSID')

merged$bmi25 <- merged$W8DBMI
merged$bmi32 <- merged$W9DBMI

merged$bmi25 <- ifelse(merged$W8DBMI == -9, -9,
                       ifelse(merged$W8DBMI == -8, -8,
                              ifelse(merged$W8DBMI == -1, -1,
                                     ifelse(is.na(merged$W8DBMI) | merged$W8DBMI < -1, -3, merged$W8DBMI))))

merged$bmi32 <- ifelse(merged$W9DBMI == -9, -9,
                       ifelse(merged$W9DBMI == -8, -8,
                              ifelse(merged$W9DBMI == -1, -1,
                                     ifelse(is.na(merged$W9DBMI) | merged$W9DBMI < -1, -3, merged$W9DBMI))))

# Create named character vectors for labels
bmi25_labels <- c('-9' = 'Refusal', '-8' = 'Don\'t know/insufficient information', 
                  '-1' = 'Item not applicable', '-3' = 'Not asked/interviewed', 
                  '-2' = 'Script error/lost')
bmi32_labels <- c('-9' = 'Refusal', '-8' = 'Don\'t know/insufficient information', 
                  '-1' = 'Item not applicable', '-3' = 'Not asked/interviewed', 
                  '-2' = 'Script error/lost')

# Add labels as attributes
attr(merged$bmi25, 'label') <- bmi25_labels
attr(merged$bmi32, 'label') <- bmi32_labels

output <- merged %>% select(NSID, bmi25, bmi32)
write_csv(output, paste0(output_dir, 'cleaned_data.csv'))

cat('Dataset created with', nrow(output), 'observations\n')
cat('Variables:', names(output), '\n')