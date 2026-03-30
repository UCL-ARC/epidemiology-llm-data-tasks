library(haven)
library(dplyr)
library(purrr)
library(readr)

dir.create('data/output', showWarnings = FALSE, recursive = TRUE)

message('Loading data files...')

wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave5 <- read_delim('data/input/wave_five_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

message('Merging by NSID...')

combined <- wave1
combined <- full_join(combined, wave4, by = 'NSID')
combined <- full_join(combined, wave5, by = 'NSID')
combined <- full_join(combined, wave6, by = 'NSID')
combined <- full_join(combined, wave7, by = 'NSID')
combined <- full_join(combined, wave8, by = 'NSID')
combined <- full_join(combined, wave9, by = 'NSID')

message('Processing NS-SEC variables...')

# Wave 4 (Age 17) - W4nsseccatYP
if (all(c('NSID', 'W4nsseccatYP') %in% names(combined))) {
  combined <- combined %>%
    mutate(
      nssec17 = case_when(
        is.na(W4nsseccatYP) ~ -3,
        W4nsseccatYP == -99 ~ -3,
        W4nsseccatYP == -91 ~ -1,
        W4nsseccatYP >= 1 & W4nsseccatYP <= 17 ~ floor(W4nsseccatYP),
        TRUE ~ NA_real_
      )
    )
}

# Wave 5 (Age 18) - W5nsseccatYP
if (all(c('NSID', 'W5nsseccatYP') %in% names(combined))) {
  combined <- combined %>%
    mutate(
      nssec18 = case_when(
        is.na(W5nsseccatYP) ~ -3,
        W5nsseccatYP == -91 ~ -1,
        W5nsseccatYP >= 1 & W5nsseccatYP <= 17 ~ floor(W5nsseccatYP),
        TRUE ~ NA_real_
      )
    )
}

# Wave 6 (Age 19) - w6nsseccatYP
if (all(c('NSID', 'w6nsseccatYP') %in% names(combined))) {
  combined <- combined %>%
    mutate(
      nssec19 = case_when(
        is.na(w6nsseccatYP) ~ -3,
        w6nsseccatYP == -91 ~ -1,
        w6nsseccatYP >= 1 & w6nsseccatYP <= 17 ~ floor(w6nsseccatYP),
        TRUE ~ NA_real_
      )
    )
}

# Wave 7 (Age 20) - W7NSSECCat
if (all(c('NSID', 'W7NSSECCat') %in% names(combined))) {
  combined <- combined %>%
    mutate(
      nssec20 = case_when(
        is.na(W7NSSECCat) ~ -3,
        W7NSSECCat == -91 ~ -1,
        W7NSSECCat >= 1 & W7NSSECCat <= 17 ~ floor(W7NSSECCat),
        TRUE ~ NA_real_
      )
    )
}

# Wave 8 (Age 25) - W8DNSSEC17 and W8DACTIVITYC
# Special: derive full-time student (15) from economic activity
if (all(c('NSID', 'W8DNSSEC17', 'W8DACTIVITYC') %in% names(combined))) {
  combined <- combined %>%
    mutate(
      # First convert activity to character for string comparison
      W8DACTIVITYC_char = as.character(W8DACTIVITYC),
      # Derive nssec25: if full-time student (activity code 5), use 15
      # Otherwise use floor of NS-SEC value
      nssec25 = case_when(
        W8DACTIVITYC_char == '5.0' ~ 15,
        is.na(W8DNSSEC17) ~ -3,
        W8DNSSEC17 == -9 ~ -9,
        W8DNSSEC17 == -8 ~ -8,
        W8DNSSEC17 == -1 ~ -1,
        W8DNSSEC17 >= 1 & W8DNSSEC17 <= 17 ~ floor(W8DNSSEC17),
        TRUE ~ NA_real_
      )
    ) %>%
    select(-W8DACTIVITYC_char)
}

# Wave 9 (Age 32) - W9NSSEC
if (all(c('NSID', 'W9NSSEC') %in% names(combined))) {
  combined <- combined %>%
    mutate(
      nssec32 = case_when(
        is.na(W9NSSEC) ~ -3,
        W9NSSEC == -9 ~ -9,
        W9NSSEC == -8 ~ -8,
        W9NSSEC == -7 ~ -7,
        W9NSSEC == -6 ~ -6,
        W9NSSEC == -5 ~ -5,
        W9NSSEC == -4 ~ -4,
        W9NSSEC == -3 ~ -3,
        W9NSSEC == -2 ~ -2,
        W9NSSEC == -1 ~ -1,
        W9NSSEC >= 1 & W9NSSEC <= 17 ~ floor(W9NSSEC),
        TRUE ~ NA_real_
      )
    )
}

message('Renaming remaining NS-SEC variables...')

# Remove original NS-SEC columns after creating new names
if ('W4nsseccatYP' %in% names(combined)) {
  combined <- combined %>% select(-W4nsseccatYP)
}
if ('W5nsseccatYP' %in% names(combined)) {
  combined <- combined %>% select(-W5nsseccatYP)
}
if ('w6nsseccatYP' %in% names(combined)) {
  combined <- combined %>% select(-w6nsseccatYP)
}
if ('W7NSSECCat' %in% names(combined)) {
  combined <- combined %>% select(-W7NSSECCat)
}
if ('W8DNSSEC17' %in% names(combined)) {
  combined <- combined %>% select(-W8DNSSEC17)
}
if ('W9NSSEC' %in% names(combined)) {
  combined <- combined %>% select(-W9NSSEC)
}

message('Converting to labelled factors...')

nssec_labels <- c(
  'Employers in large organisations',
  'Higher managerial and administrative occupations',
  'Higher professional occupations',
  'Lower professional and higher technical occupations',
  'Lower managerial and administrative occupations',
  'Higher supervisory occupations',
  'Intermediate occupations',
  'Employers in small establishments',
  'Own account workers',
  'Lower supervisory occupations',
  'Lower technical occupations',
  'Semi-routine occupations',
  'Routine occupations',
  'Never worked and Long-term unemployed',
  'Full-time students',
  'Occupations not stated or inadequately described',
  'Not classifiable for other reasons'
)

nssec_vars <- c('nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')

for (var in nssec_vars) {
  if (var %in% names(combined)) {
    var_labels <- set_names(nssec_labels, as.character(1:17))
    var_labels <- c(var_labels,
      c('-9' = 'Refusal',
        '-8' = 'Don\'t know/insufficient information',
        '-7' = 'Prefer not to say',
        '-3' = 'Not asked at the fieldwork stage/participated/interviewed',
        '-2' = 'Schedule not applicable/Script error/information lost',
        '-1' = 'Item not applicable'))
    
    combined <- combined %>%
      mutate(
        !!var := factor(
          !!var,
          levels = c(-9, -8, -7, -3, -2, -1, 1:17),
          labels = var_labels
        )
      )
  }
}

message('Selecting output columns...')

output_cols <- c('NSID', 'nssec17', 'nssec18', 'nssec19', 'nssec20', 'nssec25', 'nssec32')

existing_cols <- intersect(output_cols, names(combined))
final_output <- combined %>% select(all_of(existing_cols))

message('Writing output file...')

write_csv(final_output, 'data/output/cleaned_data.csv')

message('Done!')
