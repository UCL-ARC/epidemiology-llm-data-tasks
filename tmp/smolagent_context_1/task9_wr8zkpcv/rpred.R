library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
file1 <- "data/input/wave_one_lsype_family_background_2020.tab"
file2 <- "data/input/wave_two_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load datasets
data1 <- read_delim(file1, delim = "\t", col_types = cols(.default = "c")) %>% mutate(across(everything(), as.numeric, .names = "num_{.col}"))
# Wait, NSID should be string. Let's read properly.
data1 <- read_delim(file1, delim = "\t")
data2 <- read_delim(file2, delim = "\t")
data4 <- read_delim(file4, delim = "\t")

# Merge datasets
merged_data <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Standard Missing-Value Scheme
# -9 = Refusal
# -8 = Don't know / insufficient information
# -7 = Prefer not to say
# -3 = Not asked / not interviewed / NA
# -2 = Schedule not applicable / script error / info lost
# -1 = Item not applicable

# Mapping function based on labels provided in metadata
map_missing <- function(val, var_name) {
  if (is.na(val)) return(-3)
  
  # We use the label meanings from the metadata
  # -999.0: Missing - household data lost -> -2
  # -99.0: Not interviewed -> -3
  # -98.0: Not present -> -3
  # -94.0: Insufficient information -> -8
  # -92.0: Refused -> -9
  # -91.0: Not applicable -> -1
  # -1.0: Don't know -> -8
  
  if (val == -999) return(-2)
  if (val == -99) return(-3)
  if (val == -98) return(-3)
  if (val == -94) return(-8)
  if (val == -92) return(-9)
  if (val == -91) return(-1)
  if (val == -1) return(-8)
  
  return(val)
}

# Apply mapping to all relevant source variables
# Mother variables: W1hiqualmum, W2hiqualmum, w4hiqualmum
# Father variables: W1hiqualdad, W2hiqualdad, w4hiqualdad

# Create a helper to process parent consolidation
consolidate_edu <- function(df, vars) {
  # vars is a vector of variable names in order (W1, W2, W4)
  
  # 1. Harmonise missing codes first
  harmonised_cols <- map(vars, function(v) {
    df[[v]] %>% map_dbl(map_missing, v)
  })
  
  # Convert list to data frame for rowwise processing
  temp_df <- as.data.frame(harmonised_cols)
  colnames(temp_df) <- vars
  
  # 2. Consolidation logic
  # Take first positive value (1-20). If none, take first negative code.
  res <- apply(temp_df, 1, function(row) {
    row <- as.numeric(row)
    # First positive
    pos <- row[row > 0 & row <= 20]
    if (length(pos) > 0) return(pos[1])
    
    # First negative/missing code
    neg <- row[row < 0]
    if (length(neg) > 0) return(neg[1])
    
    return(-3)
  })
  return(res)
}

# Process Mother
mum_vars <- c("W1hiqualmum", "W2hiqualmum", "w4hiqualmum")
merged_data$mum_detailed <- consolidate_edu(merged_data, mum_vars)

# Process Father
dad_vars <- c("W1hiqualdad", "W2hiqualdad", "w4hiqualdad")
merged_data$dad_detailed <- consolidate_edu(merged_data, dad_vars)

# Collapsed mapping function
collapse_edu <- function(val) {
  if (val < 0) return(val) # Negative codes pass through
  if (val >= 1 && val <= 4) return(0)
  if (val >= 5 && val <= 17) return(1)
  if (val == 18) return(2)
  if (val == 19) return(3)
  if (val == 20) return(4)
  return(-3)
}

merged_data$mum_collapsed <- sapply(merged_data$mum_detailed, collapse_edu)
merged_data$dad_collapsed <- sapply(merged_data$dad_detailed, collapse_edu)

# Final selection
final_data <- merged_data %>%
  select(NSID, mum_collapsed, mum_detailed, dad_collapsed, dad_detailed)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")