library(haven)
library(dplyr)
library(readr)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_young_person_2020.tab', delim = '\t')
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

cat('Loaded files successfully\n')

# Check NSID distribution
w1_nsid <- w1$NSID
w2_nsid <- w2$NSID
w3_nsid <- w3$NSID
w4_nsid <- w4$NSID

cat('W1 NSID count:', length(w1_nsid), 'unique:', length(unique(w1_nsid)), '\n')
cat('W2 NSID count:', length(w2_nsid), 'unique:', length(unique(w2_nsid)), '\n')
cat('W3 NSID count:', length(w3_nsid), 'unique:', length(unique(w3_nsid)), '\n')
cat('W4 NSID count:', length(w4_nsid), 'unique:', length(unique(w4_nsid)), '\n')

# Check if there are NA values in NSID
for (i in 1:4) {
  cat(sprintf('Wave %d NA in NSID: %d\n', i, sum(is.na(c(w1$NSID, w2$NSID, w3$NSID, w4$NSID)))))
}

# Full join all datasets by NSID
full_data <- full_join(w1, w2, by = 'NSID')
full_data <- full_join(full_data, w3, by = 'NSID')
full_data <- full_join(full_data, w4, by = 'NSID')

cat('Merged data rows:', nrow(full_data), '\n')

# Check which IDs have NA for all language variables
cat('Checking language variables in full data:\n')
cat('W1englangYP in data:', 'W1englangYP' %in% names(full_data), '\n')
cat('W2EnglangYP in data:', 'W2EnglangYP' %in% names(full_data), '\n')
cat('W3englangHH in data:', 'W3englangHH' %in% names(full_data), '\n')
cat('W4EngLangHH in data:', 'W4EngLangHH' %in% names(full_data), '\n')

# Count NAs in each language variable
cat('NAs in W1englangYP:', sum(is.na(full_data$W1englangYP)), '\n')
cat('NAs in W2EnglangYP:', sum(is.na(full_data$W2EnglangYP)), '\n')
cat('NAs in W3englangHH:', sum(is.na(full_data$W3englangHH)), '\n')
cat('NAs in W4EngLangHH:', sum(is.na(full_data$W4EngLangHH)), '\n')

# Check NSID values in full data
cat('Full data NSID NAs:', sum(is.na(full_data$NSID)), '\n')
