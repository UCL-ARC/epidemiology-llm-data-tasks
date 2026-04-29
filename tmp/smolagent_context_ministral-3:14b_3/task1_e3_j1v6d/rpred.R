library(readr)
library(dplyr)

# Load first file
file1 <- 'data/input/wave_one_lsype_young_person_2020.tab'
data1 <- read_delim(file1, delim = '\t')

# Load second file
file2 <- 'data/input/wave_two_lsype_young_person_2020.tab'
data2 <- read_delim(file2, delim = '\t')

# Convert NSID to character
data1$NSID <- as.character(data1$NSID)
data2$NSID <- as.character(data2$NSID)

# Merge data
merged <- left_join(data1, data2, by = 'NSID')

# Create basic sex variable
merged$sex <- merged$W2SexYP
merged$sex[is.na(merged$sex)] <- merged$W1sexYP

# Save result
write_csv(merged[, c('NSID', 'sex')], 'data/output/cleaned_data.csv')

cat('Basic cleaning complete')