# Load required libraries
library(dplyr)
library(readr)
library(labelled)

# Ensure output directory exists
dir.create('data/output', showWarnings = FALSE)

# Load all input files
# Wave 2 (Age 15) - Family Background
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = cols())

# Wave 3 (Age 16) - Family Background
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = cols())

# Wave 8 (Age 25) - Derived
w8 <- read_delim('data/input/ns8_2015_derived.tab', delim = '\t', col_types = cols())

# Wave 9 (Age 32) - Derived Variables
w9d <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t', col_types = cols())

# Wave 9 (Age 32) - Main Interview
w9m <- read_delim('data/input/ns9_2022_main_interview.tab', delim = '\t', col_types = cols())

# Load ID-only files to preserve full cohort
w1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t', col_types = cols())
w4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t', col_types = cols())

# Check columns in each file
cat("w2 columns:", names(w2), "\n")
cat("w3 columns:", names(w3), "\n")
cat("w8 columns:", names(w8), "\n")
cat("w9d columns:", names(w9d), "\n")
cat("w9m columns:", names(w9m), "\n")

# Rename columns from w3 to avoid conflicts with w2
w3_renamed <- w3 %>%
  rename(urbind16 = urbind, gor16 = gor)

# Rename columns from w2 to avoid conflicts
w2_renamed <- w2 %>%
  rename(urbind15 = urbind, gor15 = gor)

# Merge all datasets by NSID
data <- w1 %>%
  full_join(w2_renamed, by = 'NSID') %>%
  full_join(w3_renamed, by = 'NSID') %>%
  full_join(w8, by = 'NSID') %>%
  full_join(w9d, by = 'NSID') %>%
  full_join(w9m, by = 'NSID')

# Check columns in merged data
cat("merged columns:", names(data), "\n")

# === Recode missing values for age 15 variables ===

# regub15: urbind15 from W2
# -94 -> -8 (Insufficient information/Don't know)
# -999 -> -2 (Schedule not applicable)
# NA -> -3 (Not asked)
data <- data %>%
  mutate(regub15 = urbind15) %>%
  mutate(regub15 = ifelse(regub15 == -94, -8, regub15)) %>%
  mutate(regub15 = ifelse(regub15 == -999, -2, regub15)) %>%
  mutate(regub15 = ifelse(is.na(regub15), -3, regub15))

# regov15: gor15 from W2
# -94 -> -8 (Insufficient information/Don't know)
# -999 -> -2 (Schedule not applicable)
# NA -> -3 (Not asked)
data <- data %>%
  mutate(regov15 = gor15) %>%
  mutate(regov15 = ifelse(regov15 == -94, -8, regov15)) %>%
  mutate(regov15 = ifelse(regov15 == -999, -2, regov15)) %>%
  mutate(regov15 = ifelse(is.na(regov15), -3, regov15))

# === Recode missing values for age 16 variables ===

# regub16: urbind16 from W3
# -94 -> -8 (Insufficient information/Don't know)
# -999 -> -2 (Schedule not applicable)
# NA -> -3 (Not asked)
data <- data %>%
  mutate(regub16 = urbind16) %>%
  mutate(regub16 = ifelse(regub16 == -94, -8, regub16)) %>%
  mutate(regub16 = ifelse(regub16 == -999, -2, regub16)) %>%
  mutate(regub16 = ifelse(is.na(regub16), -3, regub16))

# regov16: gor16 from W3
# -94 -> -8 (Insufficient information/Don't know)
# -999 -> -2 (Schedule not applicable)
# NA -> -3 (Not asked)
data <- data %>%
  mutate(regov16 = gor16) %>%
  mutate(regov16 = ifelse(regov16 == -94, -8, regov16)) %>%
  mutate(regov16 = ifelse(regov16 == -999, -2, regov16)) %>%
  mutate(regov16 = ifelse(is.na(regov16), -3, regov16))

# === Recode missing values for age 25 variables ===

# regor25: W8DGOR
# -9 -> -9 (Refusal)
# -8 -> -8 (Insufficient information/Don't know)
# -1 -> -1 (Not applicable)
# 13 -> -2 (Unknown due to faulty/missing postcode)
# NA -> -3 (Not asked)
data <- data %>%
  mutate(regor25 = W8DGOR) %>%
  mutate(regor25 = ifelse(regor25 == 13, -2, regor25)) %>%
  mutate(regor25 = ifelse(regor25 == -9, -9, regor25)) %>%
  mutate(regor25 = ifelse(regor25 == -8, -8, regor25)) %>%
  mutate(regor25 = ifelse(regor25 == -1, -1, regor25)) %>%
  mutate(regor25 = ifelse(is.na(regor25), -3, regor25))

# === Recode missing values for age 32 variables ===

# regor32: W9DRGN
# -9 -> -9 (Refusal)
# -8 -> -8 (Insufficient information/Don't know)
# -1 -> -1 (Not applicable)
# 13 -> -2 (Unknown due to faulty/missing postcode)
# NA -> -3 (Not asked)
data <- data %>%
  mutate(regor32 = W9DRGN) %>%
  mutate(regor32 = ifelse(regor32 == 13, -2, regor32)) %>%
  mutate(regor32 = ifelse(regor32 == -9, -9, regor32)) %>%
  mutate(regor32 = ifelse(regor32 == -8, -8, regor32)) %>%
  mutate(regor32 = ifelse(regor32 == -1, -1, regor32)) %>%
  mutate(regor32 = ifelse(is.na(regor32), -3, regor32))

# regint32: W9NATIONRES
# 1-4 (England/Scotland/Wales/Northern Ireland) -> 1 (In the UK)
# 5 (Outside UK or unknown) -> 2 (Abroad)
# -9 -> -9 (Refusal)
# -8 -> -8 (Don't know)
# -3 -> -3 (Not asked)
# -1 -> -1 (Not applicable)
data <- data %>%
  mutate(regint32 = W9NATIONRES) %>%
  mutate(regint32 = ifelse(regint32 %in% c(1,2,3,4), 1, regint32)) %>%
  mutate(regint32 = ifelse(regint32 == 5, 2, regint32)) %>%
  mutate(regint32 = ifelse(regint32 == -9, -9, regint32)) %>%
  mutate(regint32 = ifelse(regint32 == -8, -8, regint32)) %>%
  mutate(regint32 = ifelse(regint32 == -3, -3, regint32)) %>%
  mutate(regint32 = ifelse(regint32 == -1, -1, regint32)) %>%
  mutate(regint32 = ifelse(is.na(regint32), -3, regint32))

# === Create labelled factors for age 15 variables ===

# regub15 - Urban/Rural categories 1-8
urban_labels <- c('Urban >= 10k - sparse' = 1,
                  'Town & Fringe - sparse' = 2,
                  'Village - sparse' = 3,
                  'Hamlet and Isolated Dwelling - sparse' = 4,
                  'Urban >= 10k - less sparse' = 5,
                  'Town & Fringe - less sparse' = 6,
                  'Village - less sparse' = 7,
                  'Hamlet & Isolated Dwelling' = 8)

data$regub15 <- labelled(data$regub15, labels = urban_labels)

# regov15 - Government Office Region categories 1-9
gor_labels <- c('North East' = 1,
                'North West' = 2,
                'Yorkshire and The Humber' = 3,
                'East Midlands' = 4,
                'West Midlands' = 5,
                'East of England' = 6,
                'London' = 7,
                'South East' = 8,
                'South West' = 9)

data$regov15 <- labelled(data$regov15, labels = gor_labels)

# === Create labelled factors for age 16 variables ===

# regub16 - Urban/Rural categories 1-8
data$regub16 <- labelled(data$regub16, labels = urban_labels)

# regov16 - Government Office Region categories 1-9
data$regov16 <- labelled(data$regov16, labels = gor_labels)

# === Create labelled factors for age 25 variables ===

# regor25 - UK Region categories 1-12
uk_labels <- c('North East' = 1,
               'North West' = 2,
               'Yorkshire and the Humber' = 3,
               'East Midlands' = 4,
               'West Midlands' = 5,
               'East of England' = 6,
               'London' = 7,
               'South East' = 8,
               'South West' = 9,
               'Wales' = 10,
               'Scotland' = 11,
               'Northern Ireland' = 12)

data$regor25 <- labelled(data$regor25, labels = uk_labels)

# === Create labelled factors for age 32 variables ===

# regor32 - UK Region categories 1-12
data$regor32 <- labelled(data$regor32, labels = uk_labels)

# regint32 - In UK / Abroad
regint_labels <- c('In the UK' = 1, 'Abroad' = 2)

data$regint32 <- labelled(data$regint32, labels = regint_labels)

# === Select only final derived variables ===

result <- data %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# === Write output ===

write_csv(result, 'data/output/cleaned_data.csv')

cat('Done. Output written to data/output/cleaned_data.csv\n')
