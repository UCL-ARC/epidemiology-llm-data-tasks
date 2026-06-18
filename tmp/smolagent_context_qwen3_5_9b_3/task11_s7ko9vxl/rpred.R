library(haven)
library(dplyr)
library(readr)

# Load all four wave files
wave1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
wave4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

print('Files loaded successfully')

# Rename columns to avoid conflicts before joining
wave2_renamed <- wave2
names(wave2_renamed)[names(wave2_renamed) == 'NSID'] <- 'NSID_w2'

wave3_renamed <- wave3
names(wave3_renamed)[names(wave3_renamed) == 'NSID'] <- 'NSID_w3'

wave4_renamed <- wave4
names(wave4_renamed)[names(wave4_renamed) == 'NSID'] <- 'NSID_w4'

# Create master frame from wave1
all_data <- wave1

# Join wave2
all_data <- full_join(all_data, wave2_renamed, by = c('NSID' = 'NSID_w2'), keep = TRUE)

# Join wave3
all_data <- full_join(all_data, wave3_renamed, by = c('NSID' = 'NSID_w3'), keep = TRUE)

# Join wave4
all_data <- full_join(all_data, wave4_renamed, by = c('NSID' = 'NSID_w4'), keep = TRUE)

print('Merged all data successfully')
print(paste('Merged dimensions:', nrow(all_data), ncol(all_data)))

# Extract employment status variables
ecoactma14 <- all_data$W1empsmum
ecoactpa14 <- all_data$W1empsdad
ecoactma15 <- all_data$W2empsmum
ecoactpa15 <- all_data$W2empsdad
ecoactma16 <- all_data$W3empsmum
ecoactpa16 <- all_data$W3empsdad
ecoactma17 <- all_data$w4empsmum
ecoactpa17 <- all_data$w4empsdad

print('Extracted variables')

# Convert missing values: -99, -98, -996 -> -3 (not asked/not interviewed)
ecoactma14[ecoactma14 %in% c(-99, -98, -996)] <- -3
ecoactpa14[ecoactpa14 %in% c(-99, -98, -996)] <- -3
ecoactma15[ecoactma15 %in% c(-99, -98, -996)] <- -3
ecoactpa15[ecoactpa15 %in% c(-99, -98, -996)] <- -3
ecoactma16[ecoactma16 %in% c(-99, -98, -996)] <- -3
ecoactpa16[ecoactpa16 %in% c(-99, -98, -996)] <- -3
ecoactma17[ecoactma17 %in% c(-99, -98, -996)] <- -3
ecoactpa17[ecoactpa17 %in% c(-99, -98, -996)] <- -3

ecoactma14[is.na(ecoactma14)] <- -3
ecoactpa14[is.na(ecoactpa14)] <- -3
ecoactma15[is.na(ecoactma15)] <- -3
ecoactpa15[is.na(ecoactpa15)] <- -3
ecoactma16[is.na(ecoactma16)] <- -3
ecoactpa16[is.na(ecoactpa16)] <- -3
ecoactma17[is.na(ecoactma17)] <- -3
ecoactpa17[is.na(ecoactpa17)] <- -3

print('Missing value conversion done')

# Build result with only required variables
result <- data.frame(
  NSID = all_data$NSID,
  ecoactma14 = ecoactma14,
  ecoactpa14 = ecoactpa14,
  ecoactma15 = ecoactma15,
  ecoactpa15 = ecoactpa15,
  ecoactma16 = ecoactma16,
  ecoactpa16 = ecoactpa16,
  ecoactma17 = ecoactma17,
  ecoactpa17 = ecoactpa17
)

cat('Result dimensions:', nrow(result), ncol(result), '\n')

# Write output
write_csv(result, 'data/output/cleaned_data.csv')
cat('Output written successfully\n')

cat('Variables:', paste(names(result), collapse = ', '), '\n')
print(head(result))