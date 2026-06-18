library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

load_data <- function(f) {
  readr::read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(.default = 'numeric'))
}

# The ID variable NSID is string, so we must ensure it is read correctly
load_data_fixed <- function(f) {
  readr::read_delim(paste0('data/input/', f), delim = '\t', col_types = readr::cols(NSID = readr::col_character(), .default = 'numeric'))
}

data1 <- load_data_fixed('wave_one_lsype_family_background_2020.tab')
data2 <- load_data_fixed('wave_two_lsype_family_background_2020.tab')
data3 <- load_data_fixed('wave_three_lsype_family_background_2020.tab')
data4 <- load_data_fixed('wave_four_lsype_family_background_2020.tab')
data5 <- load_data_fixed('wave_five_lsype_family_background_2020.tab')

# Merge
full_df <- data1 %>%
  full_join(data2, by = 'NSID') %>%
  full_join(data3, by = 'NSID') %>%
  full_join(data4, by = 'NSID') %>%
  full_join(data5, by = 'NSID')

# Harmonisation logic for NS-SEC major categories
# 1. Employers large: 1
# 2. Higher managerial: 2
# 3. Higher professional: 3.1, 3.2, 3.3, 3.4 -> 3
# 4. Lower professional: 4.1, 4.2, 4.3, 4.4 -> 4
# 5. Lower managerial: 5
# 6. Higher supervisory: 6
# 7. Intermediate: 7.1, 7.2, 7.3, 7.4 -> 7
# 8. Small employers: 8.1, 8.2 -> 8
# 9. Own account: 9.1, 9.2 -> 9
# 10. Lower supervisory: 10
# 11. Lower technical: 11.1, 11.2 -> 11
# 12. Semi routine: 12.1-12.7 -> 12
# 13. Routine: 13.1-13.5 -> 13
# 14. Never worked/Unemployed: 14.1, 14.2, 14.3 -> 14
# 15. Full-time students: 15
# 16. Not classified: 16
# 17. Other reasons: 17

collapse_nssec <- function(x) {
  res <- rep(NA, length(x))
  
  # Valid categories
  res[x == 1.0] <- 1
  res[x == 2.0] <- 2
  res[x >= 3.1 & x <= 3.4] <- 3
  res[x >= 4.1 & x <= 4.4] <- 4
  res[x == 5.0] <- 5
  res[x == 6.0] <- 6
  res[x >= 7.1 & x <= 7.4] <- 7
  res[x >= 8.1 & x <= 8.2] <- 8
  res[x >= 9.1 & x <= 9.2] <- 9
  res[x == 10.0] <- 10
  res[x >= 11.1 & x <= 11.2] <- 11
  res[x >= 12.1 & x <= 12.7] <- 12
  res[x >= 13.1 & x <= 13.5] <- 13
  res[x >= 14.1 & x <= 14.3] <- 14
  res[x == 15.0] <- 15
  res[x == 16.0] <- 16
  res[x == 17.0] <- 17

  # Missing values mapping
  # -999.0: Missing household data lost -> -2
  # -99.0: Not interviewed -> -3
  # -98.0: Not present -> -1
  # -94.0: Insufficient information -> -8
  
  res[x == -999.0] <- -2
  res[x == -99.0] <- -3
  res[x == -98.0] <- -1
  res[x == -94.0] <- -8
  
  # Any remaining NA to -3
  res[is.na(res)] <- -3
  
  return(res)
}

# Apply to requested variables
# Wave 1 (14)
full_df <- full_df %>%
  mutate(nssecma14 = collapse_nssec(W1nsseccatmum),
         nssecpa14 = collapse_nssec(W1nsseccatdad))

# Wave 2 (15)
full_df <- full_df %>%
  mutate(nssecma15 = collapse_nssec(W2nsseccatmum),
         nssecpa15 = collapse_nssec(W2nsseccatdad))

# Wave 3 (16)
full_df <- full_df %>%
  mutate(nssecma16 = collapse_nssec(W3cnsseccatmum),
         nssecpa16 = collapse_nssec(W3cnsseccatdad))

# Wave 4 (17)
full_df <- full_df %>%
  mutate(nssecma17 = collapse_nssec(w4cnsseccatmum),
         nssecpa17 = collapse_nssec(w4cnsseccatdad))

# Wave 5 (18) - requested as nssecma18 and nssecpa18
full_df <- full_df %>%
  mutate(nssecma18 = collapse_nssec(w5Cnsseccatmum),
         nssecpa18 = collapse_nssec(w5Cnsseccatdad))

# Final selection
final_vars <- c('NSID', 'nssecma14', 'nssecpa14', 'nssecma15', 'nssecpa15', 'nssecma16', 'nssecpa16', 'nssecma17', 'nssecpa17', 'nssecma18', 'nssecpa18')
output_df <- full_df %>% select(all_of(final_vars))

# Labeling
nssec_labels <- c(
  '1' = 'Employers in large organisations',
  '2' = 'Higher managerial occupations',
  '3' = 'Higher professional',
  '4' = 'Lower professional',
  '5' = 'Lower managerial occupations',
  '6' = 'Higher supervisory occupations',
  '7' = 'Intermediate',
  '8' = 'Employers in small orgs non-professional',
  '9' = 'Own account workers non professional',
  '10' = 'Lower supervisory occupations',
  '11' = 'Lower technical craft',
  '12' = 'Semi routine',
  '13' = 'Routine',
  '14' = 'Never worked/unemployed',
  '15' = 'Full-time students',
  '16' = 'Not classified',
  '17' = 'Not classifiable for other reasons',
  '-1' = 'Item not applicable',
  '-2' = 'Schedule not applicable/information lost',
  '-3' = 'Not asked/not interviewed',
  '-8' = 'Don\'t know/insufficient information'
)

# Apply labels to all nssec variables
for(var in final_vars[final_vars != 'NSID']) {
  output_df[[var]] <- factor(output_df[[var]], levels = as.numeric(names(nssec_labels)), labels = nssec_labels)
}

readr::write_csv(output_df, 'data/output/cleaned_data.csv')
