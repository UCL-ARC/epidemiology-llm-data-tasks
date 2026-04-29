library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files and their respective variables
files_info <- list(
  wave1 = list(file = 'wave_one_lsype_family_background_2020.tab', mum = 'W1nsseccatmum', dad = 'W1nsseccatdad'),
  wave2 = list(file = 'wave_two_lsype_family_background_2020.tab', mum = 'W2nsseccatmum', dad = 'W2nsseccatdad'),
  wave3 = list(file = 'wave_three_lsype_family_background_2020.tab', mum = 'W3cnsseccatmum', dad = 'W3cnsseccatdad'),
  wave4 = list(file = 'wave_four_lsype_family_background_2020.tab', mum = 'w4cnsseccatmum', dad = 'w4cnsseccatdad'),
  wave5 = list(file = 'wave_five_lsype_family_background_2020.tab', mum = 'w5Cnsseccatmum', dad = 'w5Cnsseccatdad')
)

# NS-SEC 17-category labels
nssec_labels <- c(
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional",
  "4" = "Lower professional",
  "5" = "Lower managerial occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Small business employers",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical craft",
  "12" = "Semi routine",
  "13" = "Routine",
  "14" = "Never worked/unemployed",
  "15" = "Full-time students",
  "16" = "Not classified or inadequately stated",
  "17" = "Not classifiable for other reasons"
)

# Function to process NS-SEC variable
process_nssec <- function(x) {
  # Convert to numeric and handle NA
  val <- as.numeric(x)
  
  # Map specific missing values
  # -98 -> -3 (Parent not present)
  # Others based on metadata label meanings
  # -999, -99, -94 etc map to standard missing codes
  # -999 (household data lost) -> -2
  # -99 (not interviewed) -> -3
  # -94 (insufficient info) -> -8
  
  res <- case_when(
    val == -98 ~ -3,
    val == -999 ~ -2,
    val == -99 ~ -3,
    val == -94 ~ -8,
    val >= 1 ~ floor(val), # Collapse fractional codes to integer
    TRUE ~ -3 # Default for NA or others
  )
  
  return(res)
}

# Load and merge data
full_data <- NULL

for (wave in names(files_info)) {
  file_path <- paste0('data/input/', files_info[[wave]]$file)
  df <- readr::read_delim(file_path, delim = "\t", col_types = readr::cols(.default = "c"))
  
  if (is.null(full_data)) {
    full_data <- df
  } else {
    full_data <- full_join(full_data, df, by = "NSID")
  }
}

# Define target mapping
mapping <- list(
  nssecma14 = 'W1nsseccatmum',
  nssecpa14 = 'W1nsseccatdad',
  nssecma15 = 'W2nsseccatmum',
  nssecpa15 = 'W2nsseccatdad',
  nssecma16 = 'W3cnsseccatmum',
  nssecpa16 = 'W3cnsseccatdad',
  nssecma17 = 'w4cnsseccatmum',
  nssecpa17 = 'w4cnsseccatdad',
  nssecma18 = 'w5Cnsseccatmum',
  nssecpa18 = 'w5Cnsseccatdad'
)

# Create derived variables
final_df <- full_data %>% select(NSID)

for (target in names(mapping)) {
  source_var <- mapping[[target]]
  processed_vals <- process_nssec(full_data[[source_var]])
  
  # Create factor with labels
  # Substantive codes 1-17, Missing codes -9 to -1
  # We need to define labels for the factor levels
  levels_vals <- c(-9, -8, -7, -3, -2, -1, 1:17)
  levels_labels <- c(
    "-9" = "Refusal",
    "-8" = "Don't know / insufficient information",
    "-7" = "Prefer not to say",
    "-3" = "Not asked / not interviewed / not present",
    "-2" = "Schedule not applicable / information lost",
    "-1" = "Item not applicable",
    "1" = nssec_labels["1"],
    "2" = nssec_labels["2"],
    "3" = nssec_labels["3"],
    "4" = nssec_labels["4"],
    "5" = nssec_labels["5"],
    "6" = nssec_labels["6"],
    "7" = nssec_labels["7"],
    "8" = nssec_labels["8"],
    "9" = nssec_labels["9"],
    "10" = nssec_labels["10"],
    "11" = nssec_labels["11"],
    "12" = nssec_labels["12"],
    "13" = nssec_labels["13"],
    "14" = nssec_labels["14"],
    "15" = nssec_labels["15"],
    "16" = nssec_labels["16"],
    "17" = nssec_labels["17"]
  )
  
  final_df[[target]] <- factor(processed_vals, levels = levels_vals, labels = levels_labels)
}

# Write output
readr::write_csv(final_df, 'data/output/cleaned_data.csv')
