library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define files
files <- c(
  'wave_one_lsype_family_background_2020.tab',
  'wave_two_lsype_family_background_2020.tab',
  'wave_three_lsype_family_background_2020.tab',
  'wave_four_lsype_family_background_2020.tab',
  'wave_five_lsype_family_background_2020.tab'
)

# Load data
data_list <- map(files, ~read_delim(paste0('data/input/', .x), delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))) 
# Note: read_delim might need NSID as character, but since it's listed as string in metadata, I'll ensure it.

# The above line needs a small fix to handle NSID correctly across all files
load_file <- function(fname) {
  read_delim(paste0('data/input/', fname), delim = '\t', col_types = cols(NSID = col_character(), .default = col_double()))
}
data_list <- map(files, load_file)

# Name the list for easier access
names(data_list) <- files

# Merge datasets
full_frame <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_frame <- full_join(full_frame, data_list[[i]], by = 'NSID')
}

# Function to harmonise missing values based on metadata labels
harmonise_missing <- function(x, labels) {
  # labels is a named vector where name is the original value and value is the label
  # We map the label meaning to the standard codes
  # -9 = Refusal
  # -8 = Don't know / insufficient information
  # -7 = Prefer not to say
  # -3 = Not asked / not interviewed
  # -2 = Schedule not applicable / script error / information lost
  # -1 = Item not applicable
  
  res <- x
  
  # Identify labels and map
  # We use a helper to find which original value corresponds to which meaning
  val_to_label <- labels
  
  # Map meanings to standard codes
  # -999.0: Missing - household data lost -> -2
  # -99.0: Not interviewed -> -3
  # -98.0: Not present -> -1 (or -3, but 'not present' usually means not applicable for the question)
  # -94.0: Insufficient information -> -8
  
  # Mapping based on metadata labels provided in the prompt
  # We will iterate through the unique values of x
  unique_vals <- unique(x)
  for (v in unique_vals) {
    if (is.na(v)) next
    label <- val_to_label[as.character(v)]
    if (is.na(label)) next
    
    if (grepl('household data lost|Missing household information', label, ignore.case = TRUE)) {
      res[x == v] <- -2
    } else if (grepl('not interviewed', label, ignore.case = TRUE)) {
      res[x == v] <- -3
    } else if (grepl('not present', label, ignore.case = TRUE)) {
      res[x == v] <- -1
    } else if (grepl('Insufficient information', label, ignore.case = TRUE)) {
      res[x == v] <- -8
    } else if (grepl('Prefer not to say|Don\'t want to answer', label, ignore.case = TRUE)) {
      res[x == v] <- -7
    } else if (grepl('Refusal', label, ignore.case = TRUE)) {
      res[x == v] <- -9
    } else if (grepl('Not asked', label, ignore.case = TRUE)) {
      res[x == v] <- -3
    } else if (grepl('script error|information lost', label, ignore.case = TRUE)) {
      res[x == v] <- -2
    }
  }
  
  # Convert remaining NAs to -3
  res[is.na(res)] <- -3
  return(res)
}

# Define the mapping for each variable
# Wave 1
labels_w1_ma <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information')
labels_w1_pa <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information')

# Wave 2
labels_w2_ma <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information')
labels_w2_pa <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information')

# Wave 3
labels_w3_ma <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information')
labels_w3_pa <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information')

# Wave 4
labels_w4_ma <- c('-999.0' = 'Missing household information', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information')
labels_w4_pa <- c('-999.0' = 'Missing household information', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information')

# Wave 5
labels_w5_ma <- c('-98.0' = "Mother/father's partner not present")
labels_w5_pa <- c('-98.0' = "Father/mother's partner not present")

# Process variables
full_frame <- full_frame %>%
  mutate(
    nssecma14 = harmonise_missing(W1nsseccatmum, labels_w1_ma),
    nssecpa14 = harmonise_missing(W1nsseccatdad, labels_w1_pa),
    nssecma15 = harmonise_missing(W2nsseccatmum, labels_w2_ma),
    nssecpa15 = harmonise_missing(W2nsseccatdad, labels_w2_pa),
    nssecma16 = harmonise_missing(W3cnsseccatmum, labels_w3_ma),
    nssecpa16 = harmonise_missing(W3cnsseccatdad, labels_w3_pa),
    nssecma17 = harmonise_missing(w4cnsseccatmum, labels_w4_ma),
    nssecpa17 = harmonise_missing(w4cnsseccatdad, labels_w4_pa),
    nssecma18 = harmonise_missing(w5Cnsseccatmum, labels_w5_ma),
    nssecpa18 = harmonise_missing(w5Cnsseccatdad, labels_w5_pa)
  )

# Select final columns
final_data <- full_frame %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Write to CSV
write_csv(final_data, 'data/output/cleaned_data.csv')
