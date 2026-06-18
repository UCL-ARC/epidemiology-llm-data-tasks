library(dplyr)
library(readr)
library(labelled)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
w5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df <- full_join(w1, w2, by = "NSID")
df <- full_join(df, w3, by = "NSID")
df <- full_join(df, w4, by = "NSID")
df <- full_join(df, w5, by = "NSID")

# Define the NS-SEC collapsing function
collapse_nssec <- function(x) {
  result <- x
  
  # Map user missing values to standard codes
  result[result == -999] <- -2  # Missing - household data lost/information
  result[result == -99] <- -3   # Mother/Father not interviewed
  result[result == -98] <- -2   # Mother/Father/Partner not present
  result[result == -94] <- -8   # Insufficient information
  
  # Collapse to major NS-SEC categories (only for valid codes)
  # Take the integer part of the code
  valid_mask <- !is.na(result) & result >= 1 & result <= 17
  result[valid_mask] <- floor(result[valid_mask])
  
  return(result)
}

# Define NS-SEC labels
nssec_labels <- c(
  "-2" = "Schedule not applicable / script error / information lost",
  "-3" = "Not asked at the fieldwork stage / not interviewed",
  "-8" = "Don't know / insufficient information",
  "1" = "Employers in large organisations",
  "2" = "Higher managerial occupations",
  "3" = "Higher professional occupations",
  "4" = "Lower professional occupations",
  "5" = "Lower managerial occupations",
  "6" = "Higher supervisory occupations",
  "7" = "Intermediate occupations",
  "8" = "Employers in small organisations",
  "9" = "Own account workers",
  "10" = "Lower supervisory occupations",
  "11" = "Lower technical occupations",
  "12" = "Semi-routine occupations",
  "13" = "Routine occupations",
  "14" = "Not working / unemployed / student",
  "15" = "Full-time students",
  "16" = "Not classified or inadequately stated",
  "17" = "Not classifiable for other reasons"
)

# Define factor levels
nssec_levels <- c(-2, -3, -8, 1:17)

# Create derived variables
# Wave 1 (Age 14)
df <- df %>%
  mutate(
    nssecma14 = collapse_nssec(W1nsseccatmum),
    nssecpa14 = collapse_nssec(W1nsseccatdad)
  )

# Wave 2 (Age 15)
df <- df %>%
  mutate(
    nssecma15 = collapse_nssec(W2nsseccatmum),
    nssecpa15 = collapse_nssec(W2nsseccatdad)
  )

# Wave 3 (Age 16)
df <- df %>%
  mutate(
    nssecma16 = collapse_nssec(W3cnsseccatmum),
    nssecpa16 = collapse_nssec(W3cnsseccatdad)
  )

# Wave 4 (Age 17)
df <- df %>%
  mutate(
    nssecma17 = collapse_nssec(w4cnsseccatmum),
    nssecpa17 = collapse_nssec(w4cnsseccatdad)
  )

# Wave 5 (Age 18)
# For wave 5, we have mother's partner and father's partner variables
# nssecma18 would be NA (no mother data available)
# nssecpa18 uses w5Cnsseccatmum (mother's partner)
df <- df %>%
  mutate(
    nssecma18 = NA_real_,
    nssecpa18 = collapse_nssec(w5Cnsseccatmum)
  )

# Convert to labelled factors
nssec_factor_vars <- c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", 
                       "nssecma16", "nssecpa16", "nssecma17", "nssecpa17",
                       "nssecma18", "nssecpa18")

for (var in nssec_factor_vars) {
  df[[var]] <- factor(df[[var]], levels = nssec_levels, labels = nssec_labels)
}

# Select only ID and derived variables
output_df <- df %>% select(NSID, all_of(nssec_factor_vars))

# Write to CSV
write_csv(output_df, "data/output/cleaned_data.csv")

# Print summary
cat("Output dimensions:", dim(output_df), "\n")
cat("Columns:", names(output_df), "\n")
for (var in nssec_factor_vars) {
  cat(var, ": ", table(df[[var]], useNA = "ifany"), "\n", sep = "")
}