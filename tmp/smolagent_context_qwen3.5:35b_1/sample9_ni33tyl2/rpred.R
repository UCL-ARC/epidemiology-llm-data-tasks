# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load each file
wave_one <- read_delim(files[1], delim = "\t")
wave_two <- read_delim(files[2], delim = "\t")
wave_four <- read_delim(files[3], delim = "\t")

# Merge using full_join by NSID
df <- full_join(wave_one, wave_two, by = "NSID")
df <- full_join(df, wave_four, by = "NSID")

# Select only parental education variables and NSID
df <- df %>%
  select(NSID, W1hiqualmum, W1hiqualdad,
         W2hiqualmum, W2hiqualdad,
         w4hiqualmum, w4hiqualdad)

# Function to harmonize missing codes to standard scheme
harmonize_missing <- function(x) {
  # Map wave-specific missing codes to standard scheme
  x <- case_when(
    x == -999 ~ -9,
    x == -99 ~ -9,
    x == -98 ~ -2,
    x == -94 ~ -8,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ x
  )
  
  # Replace NA with -3 (Not asked)
  x[is.na(x)] <- -3
  
  return(x)
}

# Consolidate detailed education variables (prioritize valid positive responses)
# Order: Wave 4, Wave 2, Wave 1 (most recent first)

# Mother detailed education
df$educdtlma <- case_when(
  !is.na(df$w4hiqualmum) & df$w4hiqualmum > 0 ~ df$w4hiqualmum,
  !is.na(df$W2hiqualmum) & df$W2hiqualmum > 0 ~ df$W2hiqualmum,
  !is.na(df$W1hiqualmum) & df$W1hiqualmum > 0 ~ df$W1hiqualmum,
  TRUE ~ NA_real_
)

# Father detailed education
df$educdtlpa <- case_when(
  !is.na(df$w4hiqualdad) & df$w4hiqualdad > 0 ~ df$w4hiqualdad,
  !is.na(df$W2hiqualdad) & df$W2hiqualdad > 0 ~ df$W2hiqualdad,
  !is.na(df$W1hiqualdad) & df$W1hiqualdad > 0 ~ df$W1hiqualdad,
  TRUE ~ NA_real_
)

# Harmonize missing codes for consolidated variables
df$educdtlma <- harmonize_missing(df$educdtlma)
df$educdtlpa <- harmonize_missing(df$educdtlpa)

# Function to collapse detailed education to 5-level NVQ scheme
collapse_education <- function(x) {
  case_when(
    x %in% c(1, 2, 3, 4) ~ 0,  # NVQ 4-5: degree-level qualifications
    x %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17) ~ 1,  # NVQ 1-3: sub-degree qualifications
    x == 18 ~ 2,  # Youth training
    x == 19 ~ 3,  # Unspecified level
    x == 20 ~ 4,  # No qualification
    TRUE ~ x  # Preserve missing codes
  )
}

# Create collapsed variables
df$educma <- collapse_education(df$educdtlma)
df$educpa <- collapse_education(df$educdtlpa)

# Define value labels for detailed variables (named numeric vector)
label_detailed <- c(
  Higher_Degree = 1,
  First_Degree = 2,
  HE_Diploma = 3,
  HNC_HND_NVQ4 = 4,
  Teaching_qualification_non_degree = 5,
  Nursing_qualification_non_degree = 6,
  A_Levels = 7,
  OND_ONC = 8,
  City_and_guilds_part_III_NVQ3 = 9,
  CSYS = 10,
  Scottish_Higher_Grade = 11,
  AS_Level = 12,
  Trade_apprenticeship = 13,
  City_and_guilds_part_II_NVQ2 = 14,
  GCSE_grade_A_C_and_equivalent = 15,
  GCSE_grade_D_E_and_equivalent = 16,
  City_and_guilds_part_I_NVQ1 = 17,
  Youth_training_skill_seekers = 18,
  Qualification_level_unspecified = 19,
  No_qualification_mentioned = 20,
  Refusal = -9,
  Dont_know_insufficient_information = -8,
  Not_asked_at_the_fieldwork_stage_participated_interviewed = -3,
  Schedule_not_applicable_Script_error_information_lost = -2,
  Item_not_applicable = -1
)

# Define value labels for collapsed variables
label_collapsed <- c(
  NVQ_4_5_degree_level_qualifications_and_above = 0,
  NVQ_1_3_sub_degree_qualifications = 1,
  None_entry_training_programmes_below_NVQ_level = 2,
  Other_qualifications_where_level_is_unspecified = 3,
  No_qualifications_mentioned = 4,
  Refusal = -9,
  Dont_know_insufficient_information = -8,
  Not_asked_at_the_fieldwork_stage_participated_interviewed = -3,
  Schedule_not_applicable_Script_error_information_lost = -2,
  Item_not_applicable = -1
)

# Convert to labelled using labelled::labelled() with proper format
df$educdtlma <- labelled::labelled(df$educdtlma, label_detailed)
df$educdtlpa <- labelled::labelled(df$educdtlpa, label_detailed)
df$educma <- labelled::labelled(df$educma, label_collapsed)
df$educpa <- labelled::labelled(df$educpa, label_collapsed)

# Set variable labels
var_label(df$educdtlma) <- "Detailed maternal education (consolidated)"
var_label(df$educdtlpa) <- "Detailed paternal education (consolidated)"
var_label(df$educma) <- "Collapsed maternal education (NVQ equivalent)"
var_label(df$educpa) <- "Collapsed paternal education (NVQ equivalent)"

# Select final 5 variables
result <- df %>%
  select(NSID, educma, educpa, educdtlma, educdtlpa)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
write_csv(result, "data/output/cleaned_data.csv")

# Print summary
cat("Output file created: data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(result), "\n")
cat("Number of variables:", ncol(result), "\n")
cat("Variables:", names(result), "\n")
