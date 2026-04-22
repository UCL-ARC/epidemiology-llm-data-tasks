library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  w1 = 'data/input/wave_one_lsype_family_background_2020.tab',
  w2 = 'data/input/wave_two_lsype_family_background_2020.tab',
  w3 = 'data/input/wave_three_lsype_family_background_2020.tab',
  w4 = 'data/input/wave_four_lsype_family_background_2020.tab',
  w5 = 'data/input/wave_five_lsype_family_background_2020.tab'
)

# Load and select variables
load_and_select <- function(file_path, mum_var, dad_var, mum_new, dad_new) {
  # Load as characters to ensure NSID is preserved correctly
  df <- read_delim(file_path, delim = "\t", col_types = readr::cols(.default = "c"))
  
  # Select and rename, then convert the NS-SEC variables to numeric
  df %>% 
    select(NSID, !!sym(mum_var), !!sym(dad_var)) %>% 
    rename(!!mum_new := !!sym(mum_var), !!dad_new := !!sym(dad_var)) %>%
    mutate(across(c(mum_new, dad_new), as.numeric))
}

# Load each wave explicitly - NSID must be character to avoid precision/join issues
data1 <- load_and_select(files['w1'], 'W1nsseccatmum', 'W1nsseccatdad', 'nssecma14', 'nssecpa14')
data2 <- load_and_select(files['w2'], 'W2nsseccatmum', 'W2nsseccatdad', 'nssecma15', 'nssecpa15')
data3 <- load_and_select(files['w3'], 'W3cnsseccatmum', 'W3cnsseccatdad', 'nssecma16', 'nssecpa16')
data4 <- load_and_select(files['w4'], 'w4cnsseccatmum', 'w4cnsseccatdad', 'nssecma17', 'nssecpa17')
data5 <- load_and_select(files['w5'], 'w5Cnsseccatmum', 'w5Cnsseccatdad', 'nssecma18', 'nssecpa18')

# Ensure NSID is stripped of any whitespace and is character
process_id <- function(df) {
  df %>% mutate(NSID = trimws(as.character(NSID)))
}

data1 <- process_id(data1)
data2 <- process_id(data2)
data3 <- process_id(data3)
data4 <- process_id(data4)
data5 <- process_id(data5)

# Merge datasets using full_join
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID") %>%
  full_join(data5, by = "NSID")

# Harmonization function
harmonise_nssec <- function(x) {
  # 1. Harmonize negative codes
  x_harm <- case_when(
    x == -999 ~ -2,
    x == -94  ~ -8,
    x == -99  ~ -3,
    x == -98  ~ -3,
    is.na(x)  ~ -3,
    TRUE      ~ x
  )
  
  # 2. Handle valid occupational classes (1-17)
  res <- rep(NA, length(x_harm))
  valid_idx <- which(x_harm >= 1)
  if(length(valid_idx) > 0) {
    ints <- floor(x_harm[valid_idx])
    valid_class_idx <- valid_idx[ints >= 1 & ints <= 17]
    res[valid_class_idx] <- ints[ints >= 1 & ints <= 17]
  }
  
  # 3. Re-insert the harmonized missing codes
  missing_idx <- which(x_harm < 1)
  res[missing_idx] <- x_harm[missing_idx]
  
  return(res)
}

# Apply harmonization to all NS-SEC variables
nssec_vars <- c('nssecma14', 'nssecpa14', 'nssecma15', 'nssecpa15', 'nssecma16', 'nssecpa16', 'nssecma17', 'nssecpa17', 'nssecma18', 'nssecpa18')

merged_data <- merged_data %>%
  mutate(across(all_of(nssec_vars), harmonise_nssec))

# Define labels
labels_vec <- c(
  "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", 
  "6" = "6", "7" = "7", "8" = "8", "9" = "9", "10" = "10", 
  "11" = "11", "12" = "12", "13" = "13", "14" = "14", "15" = "15", 
  "16" = "16", "17" = "17", 
  "-9" = "Refusal", 
  "-8" = "Don't know", 
  "-7" = "Prefer not to say", 
  "-3" = "Not asked/not interviewed", 
  "-2" = "Script error/information lost", 
  "-1" = "Not applicable"
)

# Convert to labelled factors
merged_data <- merged_data %>%
  mutate(across(all_of(nssec_vars), ~ { 
    val <- . 
    factor(val, levels = as.numeric(names(labels_vec)), labels = labels_vec)
  }))

# Final variable selection and output
merged_data %>%
  select(NSID, all_of(nssec_vars)) %>%
  write_csv("data/output/cleaned_data.csv")