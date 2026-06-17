library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(labelled)

# Load all files
wf2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wf4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wf8_self <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t', show_col_types = FALSE)
wf8_derived <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', show_col_types = FALSE)
wf9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)
wf9_derived <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', show_col_types = FALSE)

cat('Files loaded successfully\n')

# Function to recode GHQ items from source coding (1-4) to Likert scores (0-3)
recode_ghq_likert <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  result <- ifelse(is.na(x_num), NA,
                   ifelse(x_num == 1, 0,
                   ifelse(x_num == 2, 1,
                   ifelse(x_num == 3, 2, 3))))
  return(result)
}

# Function to recode GHQ items for caseness (0-1)
recode_ghq_caseness <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  result <- ifelse(is.na(x_num), NA,
                   ifelse(x_num <= 2, 0, 1))
  return(result)
}

# Function to convert missing codes to standard codes
convert_missing <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  result <- x_num
  
  # Map by label meaning
  result[result == -92 | result == -9] <- -9      # Refusal
  result[result == -1.0] <- -8                     # Don't Know
  result[result == -91 | result == -1] <- -1       # Not applicable
  result[result == -99] <- -3                       # Not interviewed
  result[result == -97] <- -9                       # Refused self completion
  result[result == -998] <- -2                      # Interviewer missed question
  result[result == -997] <- -2                      # Script error
  result[result == -995] <- -2                      # Missing history section data
  result[result == -96] <- -3                       # YP using interpreter
  
  return(result)
}

# Process Wave 2 (Age 15) GHQ-12 items
wf2_ghq_vars <- c('W2concenYP', 'W2nosleepYP', 'W2usefulYP', 'W2decideYP', 
                  'W2strainYP', 'W2difficYP', 'W2activYP', 'W2probsYP', 
                  'W2depressYP', 'W2noconfYP', 'W2wthlessYP', 'W2happyYP')

# Convert missing values first
for (v in wf2_ghq_vars) {
  wf2[[v]] <- convert_missing(wf2[[v]])
}

# Recode to Likert scores (0-3)
for (i in 1:12) {
  wf2[[paste0('ghq_15_tl', i)]] <- recode_ghq_likert(wf2[[wf2_ghq_vars[i]]])
}

# Recode to caseness (0-1)
for (i in 1:12) {
  wf2[[paste0('ghq_15_c', i)]] <- recode_ghq_caseness(wf2[[wf2_ghq_vars[i]]])
}

# Compute ghqtl15: sum of Likert scores only if all 12 are non-NA and non-negative
wf2 <- wf2 %>%
  mutate(ghqtl15 = pmap_dbl(list(ghq_15_tl1, ghq_15_tl2, ghq_15_tl3, ghq_15_tl4,
                                  ghq_15_tl5, ghq_15_tl6, ghq_15_tl7, ghq_15_tl8,
                                  ghq_15_tl9, ghq_15_tl10, ghq_15_tl11, ghq_15_tl12),
                             function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) {
                               vals <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
                               if (all(!is.na(vals)) && all(vals >= 0)) {
                                 return(sum(vals))
                               } else {
                                 return(NA_real_)
                               }
                             }))

# Compute ghq15: sum of caseness scores only if all 12 are non-NA
wf2 <- wf2 %>%
  mutate(ghq15 = pmap_dbl(list(ghq_15_c1, ghq_15_c2, ghq_15_c3, ghq_15_c4,
                                ghq_15_c5, ghq_15_c6, ghq_15_c7, ghq_15_c8,
                                ghq_15_c9, ghq_15_c10, ghq_15_c11, ghq_15_c12),
                          function(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) {
                            vals <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
                            if (all(!is.na(vals))) {
                              return(sum(vals))
                            } else {
                              return(NA_real_)
                            }
                          }))

cat('Wave 2 processing complete\n')

# Process Wave 4 (Age 17) GHQ-12 items
wf4_ghq_vars <- c('W4ConcenYP', 'W4NoSleepYP', 'W4UsefulYP', 'W4DecideYP', 
                  'W4StrainYP', 'W4DifficYP', 'W4ActivYP', 'W4ProbsYP', 
                  'W4DepressYP', 'W4NoConfYP', 'W4WthlessYP', 'W4HappyYP')

# Convert missing values first
for (v in wf4_ghq_vars) {
  wf4[[v]] <- convert_missing(wf4[[v]])
}

# Recode to Likert scores (0-3)
for (i in 1:12) {
  wf4[[paste0('ghq_17_tl', i)]] <- recode_ghq_likert(wf4[[wf4_ghq_vars[i]]])
}

# Recode to caseness (0-1)
for (i in 1:12) {
  wf4[[paste0('ghq_17_c', i)]] <- recode_ghq_caseness(wf4[[wf4_ghq_vars[i]]])
}

# Compute ghqtl17
wf4 <- wf4 %>%
  mutate(ghqtl17 = pmap_dbl(list(ghq_17_tl1, ghq_17_tl2, ghq_17_tl3, ghq_17_tl4,
                                  ghq_17_tl5, ghq_17_tl6, ghq_17_tl7, ghq_17_tl8,
                                  ghq_17_tl9, ghq_17_tl10, ghq_17_tl11, ghq_17_tl12),
                             function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) {
                               vals <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
                               if (all(!is.na(vals)) && all(vals >= 0)) {
                                 return(sum(vals))
                               } else {
                                 return(NA_real_)
                               }
                             }))

# Compute ghq17
wf4 <- wf4 %>%
  mutate(ghq17 = pmap_dbl(list(ghq_17_c1, ghq_17_c2, ghq_17_c3, ghq_17_c4,
                                ghq_17_c5, ghq_17_c6, ghq_17_c7, ghq_17_c8,
                                ghq_17_c9, ghq_17_c10, ghq_17_c11, ghq_17_c12),
                          function(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) {
                            vals <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
                            if (all(!is.na(vals))) {
                              return(sum(vals))
                            } else {
                              return(NA_real_)
                            }
                          }))

cat('Wave 4 processing complete\n')

# Process Wave 8 (Age 25) GHQ-12 items
wf8_ghq_vars <- paste0('W8GHQ12_', 1:12)

# Convert missing values first
for (v in wf8_ghq_vars) {
  wf8_self[[v]] <- convert_missing(wf8_self[[v]])
}

# Recode to Likert scores (0-3)
for (i in 1:12) {
  wf8_self[[paste0('ghq_25_tl', i)]] <- recode_ghq_likert(wf8_self[[wf8_ghq_vars[i]]])
}

# Recode to caseness (0-1)
for (i in 1:12) {
  wf8_self[[paste0('ghq_25_c', i)]] <- recode_ghq_caseness(wf8_self[[wf8_ghq_vars[i]]])
}

# Compute ghqtl25
wf8_self <- wf8_self %>%
  mutate(ghqtl25 = pmap_dbl(list(ghq_25_tl1, ghq_25_tl2, ghq_25_tl3, ghq_25_tl4,
                                  ghq_25_tl5, ghq_25_tl6, ghq_25_tl7, ghq_25_tl8,
                                  ghq_25_tl9, ghq_25_tl10, ghq_25_tl11, ghq_25_tl12),
                             function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) {
                               vals <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
                               if (all(!is.na(vals)) && all(vals >= 0)) {
                                 return(sum(vals))
                               } else {
                                 return(NA_real_)
                               }
                             }))

# Compute ghq25
wf8_self <- wf8_self %>%
  mutate(ghq25 = pmap_dbl(list(ghq_25_c1, ghq_25_c2, ghq_25_c3, ghq_25_c4,
                                ghq_25_c5, ghq_25_c6, ghq_25_c7, ghq_25_c8,
                                ghq_25_c9, ghq_25_c10, ghq_25_c11, ghq_25_c12),
                          function(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) {
                            vals <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
                            if (all(!is.na(vals))) {
                              return(sum(vals))
                            } else {
                              return(NA_real_)
                            }
                          }))

cat('Wave 8 processing complete\n')

# Process Wave 9 (Age 32) GHQ-12 items
wf9_ghq_vars <- paste0('W9GHQ12_', 1:12)

# Convert missing values first
for (v in wf9_ghq_vars) {
  wf9[[v]] <- convert_missing(wf9[[v]])
}

# Recode to Likert scores (0-3)
for (i in 1:12) {
  wf9[[paste0('ghq_32_tl', i)]] <- recode_ghq_likert(wf9[[wf9_ghq_vars[i]]])
}

# Recode to caseness (0-1)
for (i in 1:12) {
  wf9[[paste0('ghq_32_c', i)]] <- recode_ghq_caseness(wf9[[wf9_ghq_vars[i]]])
}

# Compute ghqtl32
wf9 <- wf9 %>%
  mutate(ghqtl32 = pmap_dbl(list(ghq_32_tl1, ghq_32_tl2, ghq_32_tl3, ghq_32_tl4,
                                  ghq_32_tl5, ghq_32_tl6, ghq_32_tl7, ghq_32_tl8,
                                  ghq_32_tl9, ghq_32_tl10, ghq_32_tl11, ghq_32_tl12),
                             function(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) {
                               vals <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
                               if (all(!is.na(vals)) && all(vals >= 0)) {
                                 return(sum(vals))
                               } else {
                                 return(NA_real_)
                               }
                             }))

# Compute ghq32
wf9 <- wf9 %>%
  mutate(ghq32 = pmap_dbl(list(ghq_32_c1, ghq_32_c2, ghq_32_c3, ghq_32_c4,
                                ghq_32_c5, ghq_32_c6, ghq_32_c7, ghq_32_c8,
                                ghq_32_c9, ghq_32_c10, ghq_32_c11, ghq_32_c12),
                          function(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) {
                            vals <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
                            if (all(!is.na(vals))) {
                              return(sum(vals))
                            } else {
                              return(NA_real_)
                            }
                          }))

cat('Wave 9 processing complete\n')

# Now merge all datasets by NSID
cleaned <- wf2 %>%
  select(NSID, ghqtl15, ghq15) %>%
  full_join(wf4 %>% select(NSID, ghqtl17, ghq17), by = 'NSID') %>%
  full_join(wf8_self %>% select(NSID, ghqtl25, ghq25), by = 'NSID') %>%
  full_join(wf9 %>% select(NSID, ghqtl32, ghq32), by = 'NSID')

cat('Merge complete\n')
cat('Final cleaned data dimensions:', nrow(cleaned), 'rows,', ncol(cleaned), 'cols\n')

# Check for any issues with the data
cat('Summary of ghqtl15:', summary(cleaned$ghqtl15), '\n')
cat('Summary of ghqtl17:', summary(cleaned$ghqtl17), '\n')
cat('Summary of ghqtl25:', summary(cleaned$ghqtl25), '\n')
cat('Summary of ghqtl32:', summary(cleaned$ghqtl32), '\n')
cat('Summary of ghq15:', summary(cleaned$ghq15), '\n')
cat('Summary of ghq17:', summary(cleaned$ghq17), '\n')
cat('Summary of ghq25:', summary(cleaned$ghq25), '\n')
cat('Summary of ghq32:', summary(cleaned$ghq32), '\n')

# Write output
write_csv(cleaned, 'data/output/cleaned_data.csv')
cat('Output written to data/output/cleaned_data.csv\n')

# Verify output
out_check <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)
cat('Output file rows:', nrow(out_check), '\n')
cat('Output file columns:', paste(names(out_check), collapse = ', '), '\n')
print(head(out_check))
