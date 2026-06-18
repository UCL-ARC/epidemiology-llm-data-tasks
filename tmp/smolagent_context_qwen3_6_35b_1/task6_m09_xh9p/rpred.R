library(dplyr)
library(readr)
library(tidyr)
library(labelled)

# Define input path
input_path <- 'data/input/'

# Load all files
w1 <- read_delim(file.path(input_path, 'wave_one_lsype_young_person_2020.tab'), delim = '\t', show_col_types = FALSE)
w2 <- read_delim(file.path(input_path, 'wave_two_lsype_family_background_2020.tab'), delim = '\t', show_col_types = FALSE)
w3 <- read_delim(file.path(input_path, 'wave_three_lsype_family_background_2020.tab'), delim = '\t', show_col_types = FALSE)
w4 <- read_delim(file.path(input_path, 'wave_four_lsype_young_person_2020.tab'), delim = '\t', show_col_types = FALSE)
w8 <- read_delim(file.path(input_path, 'ns8_2015_derived.tab'), delim = '\t', show_col_types = FALSE)
w9d <- read_delim(file.path(input_path, 'ns9_2022_derived_variables.tab'), delim = '\t', show_col_types = FALSE)
w9m <- read_delim(file.path(input_path, 'ns9_2022_main_interview.tab'), delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
data <- full_join(w1, w2, by = 'NSID')
data <- full_join(data, w3, by = 'NSID')
data <- full_join(data, w4, by = 'NSID')
data <- full_join(data, w8, by = 'NSID')
data <- full_join(data, w9d, by = 'NSID')
data <- full_join(data, w9m, by = 'NSID')

# Now derive the variables using the correct column names
# After joining w2 and w3, urbind.x/gor.x are from w2 (age 15), urbind.y/gor.y are from w3 (age 16)
data <- data %>%
  mutate(
    # Age 15 variables from w2 (urbind.x, gor.x)
    regub15 = ifelse(is.na(urbind.x), -3, urbind.x),
    regov15 = ifelse(is.na(gor.x), -3, gor.x),
    # Age 16 variables from w3 (urbind.y, gor.y)
    regub16 = ifelse(is.na(urbind.y), -3, urbind.y),
    regov16 = ifelse(is.na(gor.y), -3, gor.y),
    # Age 25 from W8DGOR
    regor25 = ifelse(W8DGOR == 13, -2, ifelse(is.na(W8DGOR), -3, W8DGOR)),
    # Age 32 from W9DRGN
    regor32 = ifelse(W9DRGN == 13, -2, ifelse(is.na(W9DRGN), -3, W9DRGN)),
    # Age 32 from W9NATIONRES
    regint32 = case_when(
      W9NATIONRES %in% c(1, 2, 3, 4) ~ 1,
      W9NATIONRES == 5 ~ 2,
      is.na(W9NATIONRES) ~ -3,
      TRUE ~ W9NATIONRES
    )
  )

# Explicitly convert any remaining NAs to -3 for the relevant columns
data <- data %>%
  mutate(
    regub15 = ifelse(is.na(regub15), -3, regub15),
    regub16 = ifelse(is.na(regub16), -3, regub16),
    regov15 = ifelse(is.na(regov15), -3, regov15),
    regov16 = ifelse(is.na(regov16), -3, regov16),
    regor25 = ifelse(is.na(regor25), -3, regor25),
    regor32 = ifelse(is.na(regor32), -3, regor32),
    regint32 = ifelse(is.na(regint32), -3, regint32)
  )

# Select only NSID and the derived variables
output <- data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write output
write_csv(output, 'data/output/cleaned_data.csv')

cat('Output written successfully.\n')
cat('Number of rows:', nrow(output), '\n')
cat('Columns:', paste(names(output), collapse=', '), '\n')

# Verify no NAs remain
cat('\nNA counts per column:\n')
for (col in names(output)) {
  cat(col, ':', sum(is.na(output[[col]])), '\n')
}
