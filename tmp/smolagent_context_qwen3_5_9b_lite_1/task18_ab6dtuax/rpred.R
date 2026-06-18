library(haven)
library(dplyr)
library(readr)

# Load all files
wave1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

cat('Files loaded.\n')

# Convert missing values
convert_w2 <- function(x) {
  x[x == -998.0] <- -9
  x[x == -997.0] <- -9
  x[x == -995.0] <- -9
  x[x == -99.0] <- -9
  x[x == -97.0] <- -9
  x[x == -96.0] <- -9
  x[x == -92.0] <- -9
  x[x == -91.0] <- -9
  x[x == -1.0] <- -8
  x[x == -999.0] <- -9
  x
}

convert_w8 <- function(x) {
  x[x == -9.0] <- -9
  x[x == -8.0] <- -8
  x[x == -3.0] <- -3
  x[x == -1.0] <- -9
  x
}

convert_w9 <- function(x) {
  x[x == -9.0] <- -9
  x[x == -8.0] <- -8
  x[x == -3.0] <- -3
  x[x == -1.0] <- -9
  x
}

# Apply missing value conversion
w2_vars <- c('W2concenYP','W2nosleepYP','W2usefulYP','W2decideYP','W2strainYP','W2difficYP','W2activYP','W2probsYP','W2depressYP','W2noconfYP','W2wthlessYP','W2happyYP')
for(v in w2_vars) if(v %in% names(wave2)) wave2[[v]] <- as.numeric(convert_w2(wave2[[v]]))

w4_vars <- c('W4ConcenYP','W4NoSleepYP','W4UsefulYP','W4DecideYP','W4StrainYP','W4DifficYP','W4ActivYP','W4ProbsYP','W4DepressYP','W4NoConfYP','W4WthlessYP','W4HappyYP')
for(v in w4_vars) if(v %in% names(wave4)) wave4[[v]] <- as.numeric(convert_w2(wave4[[v]]))

w8_vars <- c('W8GHQ12_1','W8GHQ12_2','W8GHQ12_3','W8GHQ12_4','W8GHQ12_5','W8GHQ12_6','W8GHQ12_7','W8GHQ12_8','W8GHQ12_9','W8GHQ12_10','W8GHQ12_11','W8GHQ12_12')
for(v in w8_vars) if(v %in% names(wave8)) wave8[[v]] <- as.numeric(convert_w8(wave8[[v]]))

w9_vars <- c('W9GHQ12_1','W9GHQ12_2','W9GHQ12_3','W9GHQ12_4','W9GHQ12_5','W9GHQ12_6','W9GHQ12_7','W9GHQ12_8','W9GHQ12_9','W9GHQ12_10','W9GHQ12_11','W9GHQ12_12')
for(v in w9_vars) if(v %in% names(wave9)) wave9[[v]] <- as.numeric(convert_w9(wave9[[v]]))

cat('Missing values converted.\n')

# Compute GHQ-12 Likert and caseness
likert_score <- function(df, vars) {
  vals <- df[, vars]
  valid <- apply(vals, 1, function(r) all(r >= 1 & r <= 4 & !is.na(r)))
  if(all(valid)) rowSums(vals)
  else rep(NA_real_, nrow(df))
}

caseness_score <- function(df, vars) {
  vals <- df[, vars]
  valid <- apply(vals, 1, function(r) all(r >= 1 & r <= 4 & !is.na(r)))
  if(all(valid)) rowSums(vals >= 3)
  else rep(NA_real_, nrow(df))
}

# Compute scores
g2 <- likert_score(wave2, w2_vars)
c2 <- caseness_score(wave2, w2_vars)
wave2$ghqtl15 <- g2
wave2$ghq15 <- c2

g4 <- likert_score(wave4, w4_vars)
c4 <- caseness_score(wave4, w4_vars)
wave4$ghqtl17 <- g4
wave4$ghq17 <- c4

g8 <- likert_score(wave8, w8_vars)
c8 <- caseness_score(wave8, w8_vars)
wave8$ghqtl25 <- g8
wave8$ghq25 <- c8

g9 <- likert_score(wave9, w9_vars)
c9 <- caseness_score(wave9, w9_vars)
wave9$ghqtl32 <- g9
wave9$ghq32 <- c9

cat('Scores computed.\n')

# Merge all datasets
all_data <- wave1
all_data <- full_join(all_data, wave2, by = 'NSID')
all_data <- full_join(all_data, wave4, by = 'NSID')
all_data <- full_join(all_data, wave8, by = 'NSID')
all_data <- full_join(all_data, wave9, by = 'NSID')

cat(sprintf('All data: %d rows, unique NSIDs: %d\n', nrow(all_data), length(unique(all_data$NSID))))

# Check GHQ variables exist
cat(sprintf('ghqtl15: %s, ghq15: %s\n', 'ghqtl15' %in% names(all_data), 'ghq15' %in% names(all_data)))
cat(sprintf('ghqtl17: %s, ghq17: %s\n', 'ghqtl17' %in% names(all_data), 'ghq17' %in% names(all_data)))
cat(sprintf('ghqtl25: %s, ghq25: %s\n', 'ghqtl25' %in% names(all_data), 'ghq25' %in% names(all_data)))
cat(sprintf('ghqtl32: %s, ghq32: %s\n', 'ghqtl32' %in% names(all_data), 'ghq32' %in% names(all_data)))

# Write output
write_csv(all_data, 'data/output/cleaned_data.csv')
cat('Output written to data/output/cleaned_data.csv\n')
cat(sprintf('Variables: %d\n', ncol(all_data)))
}