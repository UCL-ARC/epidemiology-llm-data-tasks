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
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Read all files
df1 <- read_delim(files[1], delim = "\t", show_col_types = FALSE)
df2 <- read_delim(files[2], delim = "\t", show_col_types = FALSE)
df3 <- read_delim(files[3], delim = "\t", show_col_types = FALSE)
df4 <- read_delim(files[4], delim = "\t", show_col_types = FALSE)

# Rename wave 4 variables to match pattern (lowercase)
if("w4empsmum" %in% names(df4)) {
  df4 <- df4 %>% rename(W4empsmum = w4empsmum)
}
if("w4empsdad" %in% names(df4)) {
  df4 <- df4 %>% rename(W4empsdad = w4empsdad)
}

# Merge all datasets using full_join by NSID
df_merged <- full_join(df1, df2, by = "NSID")
df_merged <- full_join(df_merged, df3, by = "NSID")
df_merged <- full_join(df_merged, df4, by = "NSID")

# Function to standardize missing value codes
standardize_missing <- function(x) {
  x <- as.numeric(x)
  x <- case_when(
    x == -999 ~ -3,  # Missing household information - lost -> Not asked
    x == -99 ~ -3,   # Not interviewed -> Not asked
    x == -98 ~ -3,   # Not present -> Not asked
    x == -94 ~ -8,   # Insufficient information -> Don't know
    x == -92 ~ -9,   # Refusal -> Refusal
    x == -996 ~ -1,  # No parent in household -> Item not applicable
    is.na(x) ~ -3,   # NA -> Not asked
    TRUE ~ x
  )
  return(x)
}

# Apply standardization to parental employment variables
df_merged$W1empsmum <- standardize_missing(df_merged$W1empsmum)
df_merged$W1empsdad <- standardize_missing(df_merged$W1empsdad)
df_merged$W2empsmum <- standardize_missing(df_merged$W2empsmum)
df_merged$W2empsdad <- standardize_missing(df_merged$W2empsdad)
df_merged$W3empsmum <- standardize_missing(df_merged$W3empsmum)
df_merged$W3empsdad <- standardize_missing(df_merged$W3empsdad)
df_merged$W4empsmum <- standardize_missing(df_merged$W4empsmum)
df_merged$W4empsdad <- standardize_missing(df_merged$W4empsdad)

# Create age-specific suffixes for parental employment
df_merged$ecoactdtma14 <- df_merged$W1empsmum
df_merged$ecoactdtpa14 <- df_merged$W1empsdad
df_merged$ecoactdtma15 <- df_merged$W2empsmum
df_merged$ecoactdtpa15 <- df_merged$W2empsdad
df_merged$ecoactdtma16 <- df_merged$W3empsmum
df_merged$ecoactdtpa16 <- df_merged$W3empsdad
df_merged$ecoactdtma17 <- df_merged$W4empsmum
df_merged$ecoactdtpa17 <- df_merged$W4empsdad

# Create collapsed harmonised parental employment variables (6 categories)
# 1 = Paid work, 2 = Unemployed, 3 = Training, 4 = Education, 5 = Home/Retired/Sick, 6 = Other, -9 = Refusal, -8 = DK, -1 = N/A, -3 = Not asked
recode_collapsed <- function(x) {
  x <- as.numeric(x)
  x <- case_when(
    x %in% c(-999, -99, -98, -94, -92, -996) ~ standardize_missing(x),
    x %in% c(1, 2) ~ 1,  # Paid work
    x == 3 ~ 2,  # Unemployed
    x == 4 ~ 3,  # Training
    x == 5 ~ 4,  # Education
    x %in% c(6, 7, 8) ~ 5,  # Home/Retired/Sick
    x == 9 ~ 6,  # Other
    TRUE ~ -3
  )
  return(x)
}

df_merged$ecoactdtma14_coll <- recode_collapsed(df_merged$W1empsmum)
df_merged$ecoactdtpa14_coll <- recode_collapsed(df_merged$W1empsdad)
df_merged$ecoactdtma15_coll <- recode_collapsed(df_merged$W2empsmum)
df_merged$ecoactdtpa15_coll <- recode_collapsed(df_merged$W2empsdad)
df_merged$ecoactdtma16_coll <- recode_collapsed(df_merged$W3empsmum)
df_merged$ecoactdtpa16_coll <- recode_collapsed(df_merged$W3empsdad)
df_merged$ecoactdtma17_coll <- recode_collapsed(df_merged$W4empsmum)
df_merged$ecoactdtpa17_coll <- recode_collapsed(df_merged$W4empsdad)

# Select final variables for output
final_vars <- c(
  "NSID",
  "ecoactdtma14", "ecoactdtpa14",
  "ecoactdtma15", "ecoactdtpa15",
  "ecoactdtma16", "ecoactdtpa16",
  "ecoactdtma17", "ecoactdtpa17",
  "ecoactdtma14_coll", "ecoactdtpa14_coll",
  "ecoactdtma15_coll", "ecoactdtpa15_coll",
  "ecoactdtma16_coll", "ecoactdtpa16_coll",
  "ecoactdtma17_coll", "ecoactdtpa17_coll"
)

# Ensure output directory exists
if(!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write output
df_merged %>% select(all_of(final_vars)) %>% 
  write_csv("data/output/cleaned_data.csv")

cat("Cleaned data saved to data/output/cleaned_data.csv\n")
