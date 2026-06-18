
# Load required libraries
library(haven)
library(readr)

# Load datasets
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t')
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t')
wave8 <- read_delim('data/input/ns8_2015_self_completion.tab', delim = '\t')
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t')

# Merge datasets
merged <- wave6
merged <- merge(merged, wave7, by = 'NSID', all = TRUE)
merged <- merge(merged, wave8, by = 'NSID', all = TRUE)
merged <- merge(merged, wave9, by = 'NSID', all = TRUE)

# Create output variables with default values
merged$sori19 <- -3
merged$sori20 <- -3
merged$sori25 <- -3
merged$sori32 <- -3

# Define factor levels and labels
levels <- c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4)
labels <- c('Refusal', 'Dont know', 'Prefer not to say', 'Not asked',
            'Schedule not applicable', 'Not applicable',
            'Heterosexual/Straight', 'Gay/Lesbian', 'Bisexual', 'Other')

# Process W6SexualityYP for sori19
if ('W6SexualityYP' %in% names(merged)) {
  merged$sori19 <- ifelse(merged$W6SexualityYP %in% 1:4, merged$W6SexualityYP, -3)
  merged$sori19 <- factor(merged$sori19, levels = levels, labels = labels)
}

# Process W7SexualityYP for sori20
if ('W7SexualityYP' %in% names(merged)) {
  merged$sori20 <- ifelse(merged$W7SexualityYP %in% 1:4, merged$W7SexualityYP, -3)
  merged$sori20 <- factor(merged$sori20, levels = levels, labels = labels)
}

# Process W8SEXUALITY for sori25
if ('W8SEXUALITY' %in% names(merged)) {
  merged$sori25 <- ifelse(merged$W8SEXUALITY %in% 1:4, merged$W8SEXUALITY, -3)
  merged$sori25 <- factor(merged$sori25, levels = levels, labels = labels)
}

# Process W9SORI for sori32
if ('W9SORI' %in% names(merged)) {
  merged$sori32 <- ifelse(merged$W9SORI %in% 1:4, merged$W9SORI, -3)
  merged$sori32 <- factor(merged$sori32, levels = levels, labels = labels)
}

# Select final variables and write output
final_data <- merged[, c('NSID', 'sori19', 'sori20', 'sori25', 'sori32')]
write.csv(final_data, 'data/output/cleaned_data.csv', row.names = FALSE)
