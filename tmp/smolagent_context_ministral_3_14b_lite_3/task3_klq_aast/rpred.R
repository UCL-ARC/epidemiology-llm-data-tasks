# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load all files into separate data frames
load_file <- function(file_path) {
  read_delim(file_path, delim = "\t")
}

# Load each file with error handling
try_load <- function(file_path) {
  tryCatch({
    read_delim(file_path, delim = "\t")
  }, error = function(e) {
    message(paste("Error loading", file_path, ":", e$message))
    return(NULL)
  })
}

# Load each file
wave_one <- try_load('data/input/wave_one_lsype_young_person_2020.tab')
wave_two <- try_load('data/input/wave_two_lsype_young_person_2020.tab')
wave_three <- try_load('data/input/wave_three_lsype_family_background_2020.tab')
wave_four <- try_load('data/input/wave_four_lsype_family_background_2020.tab')

# Check if all files loaded successfully
if (any(sapply(list(wave_one, wave_two, wave_three, wave_four), is.null))) {
  stop("One or more files failed to load. Check paths and file existence.")
}

# Standardize NSID column names - convert to character and ensure consistent naming
standardize_nsid <- function(df) {
  if (!"NSID" %in% names(df)) {
    if ("nsid" %in% names(df)) {
      df$NSID <- as.character(df$nsid)
      df$nsid <- NULL
    } else if ("ID" %in% names(df)) {
      df$NSID <- as.character(df$ID)
      df$ID <- NULL
    } else {
      stop(paste("No valid ID column found in", deparse(substitute(df))))
    }
  } else {
    df$NSID <- as.character(df$NSID)
  }
  return(df)
}

# Apply standardization to all datasets
wave_one <- standardize_nsid(wave_one)
wave_two <- standardize_nsid(wave_two)
wave_three <- standardize_nsid(wave_three)
wave_four <- standardize_nsid(wave_four)

# Merge all datasets using NSID as the key
merged_data <- full_join(wave_one, wave_two, by = "NSID")
merged_data <- full_join(merged_data, wave_three, by = "NSID")
merged_data <- full_join(merged_data, wave_four, by = "NSID")

# Function to map missing values to standard codes
map_missing_values <- function(x) {
  if (!is.numeric(x)) return(x)
  
  # Create mapping vector
  missing_codes <- c(-999, -998, -997, -995, -99, -92, -91, -1)
  standard_codes <- c(-2, -2, -2, -2, -3, -9, -1, -8)
  
  for (i in seq_along(missing_codes)) {
    x[x == missing_codes[i]] <- standard_codes[i]
  }
  return(x)
}

# Map missing values for each language variable
lang_vars <- c("W1englangYP", "W2EnglangYP", "W3englangHH", "W4EngLangHH")
for (var in lang_vars) {
  if (var %in% names(merged_data)) {
    merged_data[[var]] <- map_missing_values(merged_data[[var]])
  }
}

# Consolidate language variables into a single variable 'lang'
consolidate_lang <- function(df) {
  df <- df %>%
    mutate(
      lang = case_when(
        !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
        !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
        !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
        !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
        TRUE ~ -3  # Default to not interviewed if no valid data
      )
    )

  # Create factor with labels
  levels <- c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4)
  labels <- c("Refusal", "Don't know", "Prefer not to say", 
              "Not interviewed", "Schedule not applicable", 
              "Not applicable", "English only", 
              "English first/main and speaks other languages",
              "Another language first/main", "Bilingual")
  
  df$lang <- factor(df$lang, levels = levels, labels = labels)
  return(df)
}

# Apply consolidation
merged_data <- consolidate_lang(merged_data)

# Select only the ID and the final derived variable
final_data <- merged_data %>%
  select(NSID, lang)

# Write the final output
write_csv(final_data, 'data/output/cleaned_data.csv')

# Print confirmation
message('Data cleaning and consolidation complete. Output saved to data/output/cleaned_data.csv.')