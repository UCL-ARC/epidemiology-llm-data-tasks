library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
# Note: We load all files listed in metadata to preserve cohort frame
file_paths <- c(
  'wave_one_lsype_young_person_2020.tab',
  'wave_four_lsype_young_person_2020.tab',
  'ns8_2015_derived.tab',
  'ns9_2022_derived_variables.tab'
)

# Load datasets
# Using map to read files and store in a list
data_list <- map(file_paths, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'c')))
names(data_list) <- file_paths

# Convert numeric columns for income variables specifically
data_list[['ns8_2015_derived.tab']]$W8DINCB <- as.numeric(data_list[['ns8_2015_derived.tab']]$W8DINCB)
data_list[['ns9_2022_derived_variables.tab']]$W9DINCB <- as.numeric(data_list[['ns9_2022_derived_variables.tab']]$W9DINCB)

# 2. Merge datasets
# Start with the first file and full join the rest
full_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
}

# 3. Define common labels for income bands
income_labels <- c(
  '1' = 'less than 25',
  '2' = '25 to 50',
  '3' = '50 to 90',
  '4' = '90 to 140',
  '5' = '140 to 240',
  '6' = '240 to 300',
  '7' = '300 to 350',
  '8' = '350 to 400',
  '9' = '400 to 500',
  '10' = '500 to 600',
  '11' = '600 to 700',
  '12' = '700 to 800',
  '13' = '800 to 900',
  '14' = '900 to 1200',
  '15' = '1200 to 1400',
  '16' = 'more than 1400'
)

# 4. Processing income variables
# Map missing values: -1.0 (Not applicable) -> -1
# NA -> -3 (Not asked/Not interviewed)

process_income <- function(var_vec) {
  res <- var_vec
  res[is.na(var_vec)] <- -3
  res[var_vec == -1] <- -1
  return(res)
}

# Apply processing
full_frame <- full_frame %>%
  mutate(
    inc25 = process_income(W8DINCB),
    inc32 = process_income(W9DINCB)
  )

# Convert to factors with labels
# We need to include the missing codes in the labels
all_labels <- c(
  '-1' = 'Not applicable',
  '-3' = 'Not asked at the fieldwork stage / not interviewed',
  income_labels
)

full_frame$inc25 <- factor(full_frame$inc25, levels = names(all_labels), labels = all_labels)
full_frame$inc32 <- factor(full_frame$inc32, levels = names(all_labels), labels = all_labels)

# 5. Final selection and export
final_data <- full_frame %>%
  select(NSID, inc25, inc32)

write_csv(final_data, 'data/output/cleaned_data.csv')