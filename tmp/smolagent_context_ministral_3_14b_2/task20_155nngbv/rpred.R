
library(readr)
library(dplyr)

# Load datasets
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
sweep8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
sweep9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge datasets
merged <- wave1
for (i in list(wave2, wave3, wave4, wave6, wave7, sweep8, sweep9)) {
  merged <- full_join(merged, i, by = 'NSID')
}

# Recode missing values
recode <- function(x) {
  if (!is.numeric(x)) return(x)
  x[is.na(x)] <- -3
  x[x == -999 | x == -998 | x == -997 | x == -995] <- -2
  x[x == -99] <- -3
  x[x == -97 | x == -96] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Apply recoding
vars <- c('W1alceverYP', 'W1alcmonYP', 'W2alceverYP', 'W3alceverYP', 'W4AlcEverYP',
          'W6AlcEverYP', 'W7AlcEverYP', 'W8AUDIT1', 'W9AUDIT1')
for (var in vars) {
  if (var %in% names(merged)) merged[[var]] <- recode(merged[[var]])
}

# Create drinking variables
merged$drinking_14 <- ifelse(merged$W1alceverYP == 1 & merged$W1alcmonYP == 1, 1, NA_integer_)
merged$drinking_15 <- ifelse(merged$W2alceverYP == 1, 1, NA_integer_)
merged$drinking_16 <- ifelse(merged$W3alceverYP == 1, 1, NA_integer_)
merged$drinking_17 <- ifelse(merged$W4AlcEverYP == 1, 1, NA_integer_)
merged$drinking_19 <- ifelse(merged$W6AlcEverYP == 1, 1, NA_integer_)
merged$drinking_20 <- ifelse(merged$W7AlcEverYP == 1, 1, NA_integer_)
merged$drinking_25 <- ifelse(merged$W8AUDIT1 > 1, 1, NA_integer_)
merged$drinking_32 <- ifelse(merged$W9AUDIT1 > 1, 1, NA_integer_)

# Find earliest drinking age
ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
merged$alcfst <- NA_integer_

for (age in ages) {
  var <- paste0('drinking_', age)
  if (var %in% names(merged)) {
    idx <- is.na(merged$alcfst) & !is.na(merged[[var]]) & merged[[var]] == 1
    merged$alcfst[idx] <- age
  }
}

# Handle never had alcohol cases
all_vars <- paste0('drinking_', ages)
all_vars <- all_vars[all_vars %in% names(merged)]

# Create a function to check if someone ever drank
ever_drank <- function(row) {
  any(!is.na(row[all_vars]) & row[all_vars] == 1)
}

# Check if all drinking variables are non-missing for each row
all_non_missing <- sapply(all_vars, function(x) !is.na(merged[[x]]))
all_non_missing_rows <- rowSums(all_non_missing) == length(all_vars)

# Apply never had alcohol logic
for (i in 1:nrow(merged)) {
  if (is.na(merged$alcfst[i]) && !ever_drank(merged[i, all_vars])) {
    if (all_non_missing_rows[i]) {
      merged$alcfst[i] <- 99
    } else {
      merged$alcfst[i] <- -8
    }
  }
}

# Convert to factor
levels <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)
labels <- c('Age 14', 'Age 15', 'Age 16', 'Age 17', 'Age 19', 'Age 20',
            'Age 25', 'Age 32', 'Never had alcohol', 'Don\'t know/insufficient information')
merged$alcfst <- factor(merged$alcfst, levels = levels, labels = labels)

# Select and save output
write_csv(merged[, c('NSID', 'alcfst')], 'data/output/cleaned_data.csv')
