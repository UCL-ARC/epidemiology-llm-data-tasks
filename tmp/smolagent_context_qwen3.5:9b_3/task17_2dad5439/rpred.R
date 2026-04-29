library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists('data/output')) {
  dir.create('data/output', recursive = TRUE)
}

# Read all files
df1 <- read_delim('data/input/wave_one_lsype_young_person_2020.tab', delim = '\t')
df2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
df3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t')
df4 <- read_delim('data/input/wave_four_lsype_young_person_2020.tab', delim = '\t')
df5 <- read_delim('data/input/ns9_2022_derived_variables.tab', delim = '\t')

# Function to recode missing values
recode_missing <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[x == -94] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# Extract IMDRSCORE from wave 2 (age 15)
col_imd2 <- colnames(df2)[colnames(df2) == 'IMDRSCORE']
if (length(col_imd2) == 0) col_imd2 <- colnames(df2)[ncol(df2)]

df_imd15 <- df2 %>% select(NSID, !!col_imd2) %>% rename(imd15 = !!col_imd2) %>% mutate(imd15 = recode_missing(imd15))

# Extract IMDRSCORE from wave 3 (age 16)
col_imd3 <- colnames(df3)[colnames(df3) == 'IMDRSCORE']
if (length(col_imd3) == 0) col_imd3 <- colnames(df3)[ncol(df3)]

df_imd16 <- df3 %>% select(NSID, !!col_imd3) %>% rename(imd16 = !!col_imd3) %>% mutate(imd16 = recode_missing(imd16))

# Extract W9DIMDD from ns9 (age 32)
col_imd9 <- colnames(df5)[colnames(df5) == 'W9DIMDD']
if (length(col_imd9) == 0) col_imd9 <- colnames(df5)[3]

df_imd32 <- df5 %>% select(NSID, !!col_imd9) %>% rename(imd32 = !!col_imd9) %>% mutate(imd32 = recode_missing(imd32))

# Merge all datasets by NSID
final_data <- full_join(df_imd15, df_imd16, by = 'NSID')
final_data <- full_join(final_data, df_imd32, by = 'NSID')

# Create labels as a named character vector with string names
label_names <- c('-9', '-8', '-3', '-2', '-1')
label_values <- c('Refusal', 'Don\'t know / insufficient information', 'Not asked', 'Script error', 'Not applicable')
md_labels <- setNames(label_values, label_names)

# Create proper labelled numeric variables by converting to character, then factor, then labelled, then back to numeric
# Convert to character first
final_data$imd15_char <- as.character(as.numeric(final_data$imd15))
final_data$imd16_char <- as.character(as.numeric(final_data$imd16))
final_data$imd32_char <- as.character(as.numeric(final_data$imd32))

# Get all unique values including missing codes
all_imd15_vals <- sort(unique(c(final_data$imd15_char, label_names)))
all_imd16_vals <- sort(unique(c(final_data$imd16_char, label_names)))
all_imd32_vals <- sort(unique(c(final_data$imd32_char, label_names)))

# Create factors with all possible levels
final_data$imd15_fac <- factor(final_data$imd15_char, levels = all_imd15_vals)
final_data$imd16_fac <- factor(final_data$imd16_char, levels = all_imd16_vals)
final_data$imd32_fac <- factor(final_data$imd32_char, levels = all_imd32_vals)

# Convert factors to labelled using labelled() with proper named character vector
# The key is that factor levels must match label names
tmp1 <- labelled(final_data$imd15_fac, labels = md_labels)
tmp2 <- labelled(final_data$imd16_fac, labels = md_labels)
tmp3 <- labelled(final_data$imd32_fac, labels = md_labels)

# Convert back to numeric
final_data$imd15 <- as.numeric(tmp1)
final_data$imd16 <- as.numeric(tmp2)
final_data$imd32 <- as.numeric(tmp3)

# Select only required variables
final_data <- final_data %>% select(NSID, imd15, imd16, imd32)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')

cat('Dataset successfully cleaned and saved to data/output/cleaned_data.csv\n')
cat('Variables in output:', paste(colnames(final_data), collapse=', '), '\n')
cat('Number of rows:', nrow(final_data), '\n')