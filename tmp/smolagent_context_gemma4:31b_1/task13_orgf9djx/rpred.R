library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. File Loading
# Define files
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

# Read each file and select only necessary variables
# Wave 1
d1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, nssecma14 = W1nsseccatmum, nssecpa14 = W1nsseccatdad)

# Wave 2
d2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, nssecma15 = W2nsseccatmum, nssecpa15 = W2nsseccatdad)

# Wave 3
d3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, nssecma16 = W3cnsseccatmum, nssecpa16 = W3cnsseccatdad)

# Wave 4
d4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, nssecma17 = w4cnsseccatmum, nssecpa17 = w4cnsseccatdad)

# Wave 5
d5 <- read_delim('data/input/wave_five_lsype_family_background_2020.tab', delim = '\t', col_types = readr::cols()) %>%
  select(NSID, nssecma18 = w5Cnsseccatmum, nssecpa18 = w5Cnsseccatdad)

# Merge all datasets using full_join
df <- d1 %>%
  full_join(d2, by = 'NSID') %>%
  full_join(d3, by = 'NSID') %>%
  full_join(d4, by = 'NSID') %>%
  full_join(d5, by = 'NSID')

# 4 & 5. NS-SEC Classification and Missing Value Harmonization
harmonise_nssec <- function(x) {
  # Initial copy
  res <- x
  
  # Harmonise missing codes before collapsing integer part
  # -999 -> -2
  res[x == -999] <- -2
  # -94 -> -8
  res[x == -94] <- -8
  # -99, -98, NA -> -3
  res[x == -99 | x == -98 | is.na(x)] <- -3
  
  # Now handle the occupational classes (1-17)
  # We only want to take the integer part of values >= 1
  # For values < 1 (missing codes), we keep them as is
  
  # Create a temporary vector for processing valid classes
  valid_idx <- which(res >= 1)
  if(length(valid_idx) > 0) {
    # Collapse subcategories by taking the integer part
    vals <- floor(res[valid_idx])
    # Accept only categories 1-17
    vals[vals < 1 | vals > 17] <- NA
    res[valid_idx] <- vals
  }
  
  # Final pass for any new NAs (from invalid ranges) to be -3
  res[is.na(res)] <- -3
  
  return(res)
}

# Apply harmonization to all nssec variables
nssec_vars <- c('nssecma14', 'nssecpa14', 'nssecma15', 'nssecpa15', 
                'nssecma16', 'nssecpa16', 'nssecma17', 'nssecpa17', 
                'nssecma18', 'nssecpa18')

df <- df %>%
  mutate(across(all_of(nssec_vars), harmonise_nssec))

# 7. Factor Variables and Labels
# Define the labels map
labels_map <- c(
  '1' = '1',
  '2' = '2',
  '3' = '3',
  '4' = '4',
  '5' = '5',
  '6' = '6',
  '7' = '7',
  '8' = '8',
  '9' = '9',
  '10' = '10',
  '11' = '11',
  '12' = '12',
  '13' = '13',
  '14' = '14',
  '15' = '15',
  '16' = '16',
  '17' = '17',
  '-9' = 'Refusal',
  '-8' = 'Don\'t know',
  '-7' = 'Prefer not to say',
  '-3' = 'Not asked/not interviewed',
  '-2' = 'Script error/information lost',
  '-1' = 'Not applicable'
)

# Function to convert to labelled factor
make_labelled_factor <- function(x) {
  # Ensure it's numeric/integer first
  x <- as.numeric(x)
  # Use labelled package to apply labels
  val_labels <- setNames(labels_map, names(labels_map))
  # Since we need a factor with explicit labels for the output CSV, 
  # and haven's labelled vectors are for SPSS/Stata, 
  # we will convert to a standard R factor where the levels are the labels
  
  # Mapping values to labels
  factor_vec <- factor(x, levels = as.numeric(names(labels_map)), labels = labels_map)
  return(factor_vec)
}

df <- df %>%
  mutate(across(all_of(nssec_vars), make_labelled_factor))

# 9. Output Requirements
# Ensure only required columns are present
df_final <- df %>%
  select(NSID, all_of(nssec_vars))

write_csv(df_final, 'data/output/cleaned_data.csv')