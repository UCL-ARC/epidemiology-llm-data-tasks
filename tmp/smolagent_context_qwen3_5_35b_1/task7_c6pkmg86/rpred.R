library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load data files
wave4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave6 <- read_delim('data/input/wave_six_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave7 <- read_delim('data/input/wave_seven_lsype_young_person_2020.tab', delim = '\t', show_col_types = FALSE)
wave8 <- read_delim('data/input/ns8_2015_main_interview.tab', delim = '\t', show_col_types = FALSE)
wave9 <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', show_col_types = FALSE)

# Merge all data by NSID
data <- full_join(wave4, wave6, by = 'NSID')
data <- full_join(data, wave7, by = 'NSID')
data <- full_join(data, wave8, by = 'NSID')
data <- full_join(data, wave9, by = 'NSID')

# Age 17: w4saim
educaim17 <- data$w4saim

# Map substantive codes
educaim17[educaim17 %in% c(1, 5, 9)] <- 1  # NVQ 3, 2, 1 -> NVQ 1-3
educaim17[educaim17 %in% c(2, 3, 4, 6, 7, 8, 10, 11)] <- 1  # AVCE, A/AS, Other level 3, Int GNVQ, Other level 2, GCSE, Foundation, Other level 1 -> NVQ 1-3
educaim17[educaim17 == 12] <- 3  # Other -> Other
educaim17[educaim17 == 13] <- 3  # No detail -> Other
educaim17[educaim17 == 14] <- 5  # Not studying -> Not currently studying

# Handle missing values
educaim17[educaim17 %in% c(-999, -998, -997, -995)] <- -2
educaim17[educaim17 == -92] <- -9
educaim17[educaim17 == -94] <- -8
educaim17[educaim17 == -99] <- -3
educaim17[educaim17 == -91] <- -1
educaim17[educaim17 == -97] <- -3
educaim17[educaim17 == -100] <- -3
educaim17[is.na(educaim17)] <- -3

# Age 19: W6Saim
educaim19 <- data$W6Saim

# NVQ 4-5 (category 0)
educaim19[educaim19 %in% c(1, 3)] <- 0  # NVQ 5, NVQ 4
educaim19[educaim19 %in% c(2, 4)] <- 0  # First/Other Degree, Other HE

# NVQ 1-3 (category 1)
educaim19[educaim19 %in% c(5, 9, 12)] <- 1  # NVQ 3, 2, 1
educaim19[educaim19 %in% c(6, 7, 8, 10, 11)] <- 1  # AVCE, A/AS, Other level 3, Other level 2, GCSE
educaim19[educaim19 == 13] <- 1  # Other level 1

# Other (category 3)
educaim19[educaim19 == 14] <- 3  # Other (level unknown)
educaim19[educaim19 == 15] <- 3  # No detail

# Not studying (category 5)
educaim19[educaim19 == 16] <- 5

# Missing values
educaim19[educaim19 %in% c(-999, -998, -997, -995)] <- -2
educaim19[educaim19 == -92] <- -9
educaim19[educaim19 == -94] <- -8
educaim19[educaim19 == -99] <- -3
educaim19[educaim19 == -91] <- -1
educaim19[educaim19 == -97] <- -3
educaim19[educaim19 == -100] <- -3
educaim19[is.na(educaim19)] <- -3

# Age 20: W7SAim
educaim20 <- data$W7SAim

# NVQ 4-5 (category 0)
educaim20[educaim20 %in% c(10, 11, 12, 13)] <- 0  # NVQ 4, First/Other Degree, Other HE, NVQ 5

# NVQ 1-3 (category 1)
educaim20[educaim20 %in% c(1, 3, 6)] <- 1  # NVQ 1, NVQ 2, NVQ 3
educaim20[educaim20 %in% c(2, 4, 5)] <- 1  # Other level 1, GCSE, Other level 2
educaim20[educaim20 %in% c(7, 8, 9)] <- 1  # A/AS, AVCE, Other level 3

# Other (category 3)
educaim20[educaim20 == 14] <- 3  # Other (level unknown)

# Not studying (category 5)
educaim20[educaim20 == -91] <- 5  # Not applicable (not studying)

# Missing values
educaim20[educaim20 == -94] <- -8  # Insufficient information
educaim20[educaim20 %in% c(-999, -998, -997, -995)] <- -2
educaim20[educaim20 == -92] <- -9
educaim20[educaim20 == -99] <- -3
educaim20[educaim20 == -97] <- -3
educaim20[educaim20 == -100] <- -3
educaim20[is.na(educaim20)] <- -3

# Age 25: ns8
educaim25 <- rep(-3, nrow(data))

# Check if studying
is_studying_25 <- data$W8ACTIVITY05 == 1

# Initialize with -3 (not asked)
educaim25[!is_studying_25 & data$W8ACTIVITY05 == 0] <- -3
educaim25[data$W8ACTIVITY05 == -1] <- -1  # Not applicable
educaim25[data$W8ACTIVITY05 == -8] <- -8  # Don't know
educaim25[data$W8ACTIVITY05 == -9] <- -9  # Refused

# For those studying, check qualifications
studying_25_idx <- which(is_studying_25)

# Check NVQ 4-5 (category 0)
has_nvq45_25 <- data$W8VCQUC0J == 1 | data$W8VCQUC0K == 1
educaim25[studying_25_idx[has_nvq45_25[studying_25_idx]]] <- 0

# Check NVQ 1-3 (category 1)
has_nvq13_25 <- data$W8VCQUC0E == 1 | data$W8VCQUC0D == 1 | data$W8VCQUC0B == 1 | data$W8VCQUC0C == 1 | data$W8VCQUC0A == 1
has_academic_25 <- data$W8ACQUC0F == 1 | data$W8ACQUC0G == 1 | data$W8ACQUC0H == 1 | data$W8ACQUC0I == 1 | data$W8ACQUC0J == 1 | data$W8ACQUC0K == 1 | data$W8ACQUC0L == 1 | data$W8ACQUC0M == 1 | data$W8ACQUC0N == 1
has_nvq13_25_full <- has_nvq13_25 | has_academic_25
educaim25[studying_25_idx[has_nvq13_25_full[studying_25_idx] & !has_nvq45_25[studying_25_idx]]] <- 1

# Check other (category 3)
has_other_25 <- data$W8ACQUC0O == 1 | data$W8ACQUC0P == 1 | data$W8ACQUC0Q == 1
educaim25[studying_25_idx[has_other_25[studying_25_idx] & !has_nvq45_25[studying_25_idx] & !has_nvq13_25_full[studying_25_idx]]] <- 3

# Check none of these (category 4)
has_none_25 <- data$W8ACQUC0O == 1
educaim25[studying_25_idx[has_none_25[studying_25_idx] & !has_nvq45_25[studying_25_idx] & !has_nvq13_25_full[studying_25_idx] & !has_other_25[studying_25_idx]]] <- 4

# Handle missing codes for qualification variables
educaim25[data$W8ACQUC0P == 1] <- -8  # Don't know
educaim25[data$W8ACQUC0Q == 1] <- -9  # Refused

# Age 32: ns9
educaim32 <- rep(-3, nrow(data))

# Check if studying
is_studying_32 <- data$W9ECONACT2 %in% c(6, 7)

# Not currently studying (category 5)
educaim32[!is_studying_32 & data$W9ECONACT2 %in% c(1:5, 8:14)] <- 5

# Not applicable
educaim32[data$W9ECONACT2 == -1] <- -1

# Don't know
educaim32[data$W9ECONACT2 == -8] <- -8

# Refused
educaim32[data$W9ECONACT2 == -9] <- -9

# Not asked
educaim32[data$W9ECONACT2 == -3] <- -3

# For those studying, check qualifications
studying_32_idx <- which(is_studying_32)

# Check NVQ 4-5 (category 0)
has_nvq45_32 <- data$W9ACQUC0A == 1 | data$W9ACQUC0B == 1 | data$W9ACQUC0C == 1 | data$W9ACQUC0D == 1 | data$W9ACQUC0E == 1 |
                data$W9VCQUC0A == 1 | data$W9VCQUC0C == 1 | data$W9VCQUCAC == 1

educaim32[studying_32_idx[has_nvq45_32[studying_32_idx]]] <- 0

# Check NVQ 1-3 (category 1)
has_nvq13_32 <- data$W9ACQUC0F == 1 | data$W9ACQUC0G == 1 | data$W9ACQUC0H == 1 | data$W9ACQUC0I == 1 | data$W9ACQUC0J == 1 | data$W9ACQUC0K == 1 |
                data$W9ACQUC0L == 1 | data$W9ACQUC0M == 1 | data$W9ACQUC0N == 1 | data$W9ACQUC0O == 1 | data$W9ACQUC0P == 1 | data$W9ACQUC0Q == 1 |
                data$W9VCQUC0B == 1 | data$W9VCQUC0D == 1 | data$W9VCQUC0E == 1 | data$W9VCQUC0F == 1 | data$W9VCQUC0G == 1 | data$W9VCQUC0H == 1 |
                data$W9VCQUC0I == 1 | data$W9VCQUC0J == 1 | data$W9VCQUC0K == 1 | data$W9VCQUC0L == 1 | data$W9VCQUC0M == 1 | data$W9VCQUC0N == 1 |
                data$W9VCQUC0O == 1 | data$W9VCQUC0P == 1 | data$W9VCQUC0Q == 1 | data$W9VCQUC0R == 1 | data$W9VCQUC0S == 1 | data$W9VCQUC0T == 1 |
                data$W9VCQUC0U == 1 | data$W9VCQUC0V == 1 | data$W9VCQUC0W == 1 | data$W9VCQUC0X == 1 | data$W9VCQUC0Y == 1 | data$W9VCQUC0Z == 1 |
                data$W9VCQUCAA == 1 | data$W9VCQUCAB == 1 | data$W9VCQUCAC == 1 | data$W9VCQUCAD == 1 | data$W9VCQUCAE == 1 | data$W9VCQUCAD == 1

educaim32[studying_32_idx[has_nvq13_32[studying_32_idx] & !has_nvq45_32[studying_32_idx]]] <- 1

# Check other (category 3)
has_other_32 <- data$W9ACQUC0R == 1 | data$W9VCQUCAD == 1
educaim32[studying_32_idx[has_other_32[studying_32_idx] & !has_nvq45_32[studying_32_idx] & !has_nvq13_32[studying_32_idx]]] <- 3

# Check none of these (category 4)
has_none_32 <- data$W9ACQUC0S == 1 | data$W9VCQUCAG == 1
educaim32[studying_32_idx[has_none_32[studying_32_idx] & !has_nvq45_32[studying_32_idx] & !has_nvq13_32[studying_32_idx] & !has_other_32[studying_32_idx]]] <- 4

# Handle missing codes
educaim32[data$W9ACQUC0T == 1 | data$W9VCQUCAH == 1] <- -8  # Don't know
educaim32[data$W9ACQUC0U == 1 | data$W9VCQUCAI == 1] <- -9  # Refused

# Create final dataframe with only required variables
data$educaim17 <- educaim17
data$educaim19 <- educaim19
data$educaim20 <- educaim20
data$educaim25 <- educaim25
data$educaim32 <- educaim32

result <- data[, c('NSID', 'educaim17', 'educaim19', 'educaim20', 'educaim25', 'educaim32')]

# Write to CSV
write_csv(result, 'data/output/cleaned_data.csv')

cat('Successfully created cleaned_data.csv with', nrow(result), 'rows and', ncol(result), 'columns\n')
cat('Sample of output:\n')
print(head(result))
