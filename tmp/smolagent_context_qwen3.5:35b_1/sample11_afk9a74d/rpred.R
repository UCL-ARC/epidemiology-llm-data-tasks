library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the four wave files
df14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
df_merged <- full_join(df14, df15, by = "NSID")
df_merged <- full_join(df_merged, df16, by = "NSID")
df_merged <- full_join(df_merged, df17, by = "NSID")

# Function to standardize missing value codes
standardize_missing <- function(x) {
  x[x == -999] <- -3
  x[x == -99] <- -3
  x[x == -98] <- -3
  x[x == -94] <- -8
  x[x == -996] <- -3
  x[x == -92] <- -9
  x[is.na(x)] <- -3
  return(x)
}

# Function to recode employment status to 6-category scheme
recode_employment <- function(x) {
  case_when(
    x %in% c(1) ~ 1,  # Paid work 30+ hours
    x %in% c(2) ~ 1,  # Paid work <30 hours
    x %in% c(3) ~ 2,  # Unemployed
    x %in% c(4) ~ 3,  # Training
    x %in% c(5) ~ 4,  # Education
    x %in% c(6) ~ 5,  # Looking after home
    x %in% c(7, 8, 9) ~ 6,  # Retired, Sick, Other
    TRUE ~ NA_real_
  )
}

# Apply standardization and recoding to mother's employment
df_merged$ecoactdtma14 <- standardize_missing(df_merged$W1empsmum)
df_merged$ecoactdtma14 <- recode_employment(df_merged$ecoactdtma14)

df_merged$ecoactdtma15 <- standardize_missing(df_merged$W2empsmum)
df_merged$ecoactdtma15 <- recode_employment(df_merged$ecoactdtma15)

df_merged$ecoactdtma16 <- standardize_missing(df_merged$W3empsmum)
df_merged$ecoactdtma16 <- recode_employment(df_merged$ecoactdtma16)

df_merged$ecoactdtma17 <- standardize_missing(df_merged$w4empsmum)
df_merged$ecoactdtma17 <- recode_employment(df_merged$ecoactdtma17)

# Apply standardization and recoding to father's employment
df_merged$ecoactdtpa14 <- standardize_missing(df_merged$W1empsdad)
df_merged$ecoactdtpa14 <- recode_employment(df_merged$ecoactdtpa14)

df_merged$ecoactdtpa15 <- standardize_missing(df_merged$W2empsdad)
df_merged$ecoactdtpa15 <- recode_employment(df_merged$ecoactdtpa15)

df_merged$ecoactdtpa16 <- standardize_missing(df_merged$W3empsdad)
df_merged$ecoactdtpa16 <- recode_employment(df_merged$ecoactdtpa16)

df_merged$ecoactdtpa17 <- standardize_missing(df_merged$w4empsdad)
df_merged$ecoactdtpa17 <- recode_employment(df_merged$ecoactdtpa17)

# Select only the variables to output (ID + derived variables)
df_output <- df_merged %>%
  select(NSID,
         ecoactdtma14, ecoactdtma15, ecoactdtma16, ecoactdtma17,
         ecoactdtpa14, ecoactdtpa15, ecoactdtpa16, ecoactdtpa17)

# Convert to labeled factors with explicit labels
label_employment <- function(x) {
  factor(x, levels = c(1, 2, 3, 4, 5, 6, -3, -8, -9),
         labels = c("Paid work 30+ hrs", "Paid work <30 hrs", "Unemployed", 
                    "Training", "Education", "Home/Retired/Sick/Other", 
                    "Not asked", "Don't know", "Refusal"))
}

df_output <- df_output %>%
  mutate(across(c(ecoactdtma14, ecoactdtma15, ecoactdtma16, ecoactdtma17,
                  ecoactdtpa14, ecoactdtpa15, ecoactdtpa16, ecoactdtpa17), label_employment))

# Write output to CSV
write_csv(df_output, "data/output/cleaned_data.csv")

cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")
