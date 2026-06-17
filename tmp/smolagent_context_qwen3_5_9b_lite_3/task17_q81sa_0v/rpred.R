library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load wave_two and create imd15
wave_two <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
cat('wave_two loaded, columns:', ncol(wave_two), '\n')
cat('IMDRSCORE in wave_two:', 'IMDRSCORE' %in% names(wave_two), '\n')

wave_two_valid <- wave_two$IMDRSCORE[wave_two$IMDRSCORE >= 0 & !is.na(wave_two$IMDRSCORE)]
cat('Valid IMDRSCORE values:', length(wave_two_valid), '\n')

decile2 <- rank(wave_two_valid) / length(wave_two_valid) * 10
names(decile2) <- as.character(wave_two_valid)
imd2_lookup <- setNames(round(decile2), as.character(wave_two_valid))

# Add NSID and imd15 to wave_two
wave_two$imd15 <- NA_integer_
valid_idx <- !is.na(wave_two$IMDRSCORE) & wave_two$IMDRSCORE >= 0 & wave_two$IMDRSCORE <= 365
if(any(valid_idx)) {
  wave_two$imd15[valid_idx] <- as.integer(imd2_lookup[as.character(wave_two$IMDRSCORE[valid_idx])])
}
cat('imd15 created, valid:', sum(!is.na(wave_two$imd15)), '\n')

# Load wave_three and create imd16
wave_three <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
cat('\nwave_three loaded, columns:', ncol(wave_three), '\n')
cat('IMDRSCORE in wave_three:', 'IMDRSCORE' %in% names(wave_three), '\n')

wave_three_valid <- wave_three$IMDRSCORE[wave_three$IMDRSCORE >= 0 & !is.na(wave_three$IMDRSCORE)]
cat('Valid IMDRSCORE values:', length(wave_three_valid), '\n')

decile3 <- rank(wave_three_valid) / length(wave_three_valid) * 10
names(decile3) <- as.character(wave_three_valid)
imd3_lookup <- setNames(round(decile3), as.character(wave_three_valid))

# Add NSID and imd16 to wave_three
wave_three$imd16 <- NA_integer_
valid_idx3 <- !is.na(wave_three$IMDRSCORE) & wave_three$IMDRSCORE >= 0 & wave_three$IMDRSCORE <= 365
if(any(valid_idx3)) {
  wave_three$imd16[valid_idx3] <- as.integer(imd3_lookup[as.character(wave_three$IMDRSCORE[valid_idx3])])
}
cat('imd16 created, valid:', sum(!is.na(wave_three$imd16)), '\n')

# Load ns9
ns9 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')
cat('\nns9 loaded, columns:', ncol(ns9), '\n')
cat('W9DIMDD in ns9:', 'W9DIMDD' %in% names(ns9), '\n')

# Add imd32 to ns9
ns9$imd32 <- as.integer(ns9$W9DIMDD)
cat('imd32 created, valid:', sum(!is.na(ns9$imd32)), '\n')

# Load wave_one (has NSID only)
wave_one <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
cat('\nwave_one loaded, columns:', ncol(wave_one), '\n')

# Merge: start with wave_one, add imd15 from wave_two, imd16 from wave_three, imd32 from ns9
all_data <- full_join(wave_one, wave_two, by = 'NSID')
cat('\nAfter joining wave_one + wave_two:', nrow(all_data), 'rows\n')

all_data <- full_join(all_data, wave_three, by = 'NSID')
cat('After joining + wave_three:', nrow(all_data), 'rows\n')

all_data <- full_join(all_data, ns9, by = 'NSID')
cat('After joining + ns9:', nrow(all_data), 'rows\n')

# Check IMD variables
cat('\nIMD variables in final data:\n')
cat('imd15:', 'imd15' %in% names(all_data), '\n')
cat('imd16:', 'imd16' %in% names(all_data), '\n')
cat('imd32:', 'imd32' %in% names(all_data), '\n')

if('imd15' %in% names(all_data)) {
  cat('imd15 distribution:\n')
  print(table(all_data$imd15, useNA = 'ifany'))
}
if('imd16' %in% names(all_data)) {
  cat('imd16 distribution:\n')
  print(table(all_data$imd16, useNA = 'ifany'))
}
if('imd32' %in% names(all_data)) {
  cat('imd32 distribution:\n')
  print(table(all_data$imd32, useNA = 'ifany'))
}

# Write output
output <- all_data %>% select(NSID, imd15, imd16, imd32)
write_csv(output, 'data/output/cleaned_data.csv')
cat('\nOutput written to data/output/cleaned_data.csv\n')