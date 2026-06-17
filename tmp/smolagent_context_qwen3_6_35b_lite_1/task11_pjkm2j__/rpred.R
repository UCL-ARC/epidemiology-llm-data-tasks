library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(haven)
library(labelled)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Load all files from the metadata
# Wave 1 (Age 14)
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 2 (Age 15)
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 3 (Age 16)
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Wave 4 (Age 17)
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID using full_join
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

cat("Merged dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

# Function to harmonize economic activity variable
harmonize_economic_activity <- function(x) {
  result <- x
  
  # Map specific missing codes based on label meaning
  result[result == -999] <- -2
  result[result == -996] <- -2
  result[result == -99] <- -2
  result[result == -98] <- -2
  result[result == -94] <- -8
  result[result == -92] <- -9
  
  # Convert remaining NAs to -3 (not asked)
  result[is.na(result)] <- -3
  
  return(result)
}

# Create the harmonized variables for each wave
# Wave 1 (Age 14)
df$ecoactma14 <- harmonize_economic_activity(df$W1empsmum)
df$ecoactpa14 <- harmonize_economic_activity(df$W1empsdad)

# Wave 2 (Age 15)
df$ecoactma15 <- harmonize_economic_activity(df$W2empsmum)
df$ecoactpa15 <- harmonize_economic_activity(df$W2empsdad)

# Wave 3 (Age 16)
df$ecoactma16 <- harmonize_economic_activity(df$W3empsmum)
df$ecoactpa16 <- harmonize_economic_activity(df$W3empsdad)

# Wave 4 (Age 17)
df$ecoactma17 <- harmonize_economic_activity(df$w4empsmum)
df$ecoactpa17 <- harmonize_economic_activity(df$w4empsdad)

# Select only NSID and the derived variables
df_out <- df %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15,
         ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to CSV
write_csv(df_out, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Output dimensions:", nrow(df_out), "rows,", ncol(df_out), "columns\n")

# Print summary of each variable for verification
for (var_name in names(df_out)[-1]) {
  cat("\n", var_name, ":\n")
  print(table(df_out[[var_name]], useNA = "ifany"))
}
