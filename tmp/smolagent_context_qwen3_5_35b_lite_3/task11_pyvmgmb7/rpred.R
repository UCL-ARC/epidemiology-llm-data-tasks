# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Define file paths
files <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

# Load each file explicitly by name into separate objects
df14 <- read_delim(paste0("data/input/", files[1]), delim = "\t", show_col_types = FALSE)
df15 <- read_delim(paste0("data/input/", files[2]), delim = "\t", show_col_types = FALSE)
df16 <- read_delim(paste0("data/input/", files[3]), delim = "\t", show_col_types = FALSE)
df17 <- read_delim(paste0("data/input/", files[4]), delim = "\t", show_col_types = FALSE)

# Merge datasets using full_join by NSID
df_merged <- full_join(df14, df15, by = "NSID", suffix = c("_14", "_15"))
df_merged <- full_join(df_merged, df16, by = "NSID", suffix = c("", "_16"))
df_merged <- full_join(df_merged, df17, by = "NSID", suffix = c("", "_17"))

# Function to map raw values to standard missing codes and clean
clean_ecoact <- function(x, source_name) {
  # Map to standard missing codes based on label meaning
  # -999: Missing household information -> -2
  # -99: Not interviewed -> -3
  # -98: Not present -> -3
  # -94: Insufficient information -> -8
  # -92: Refusal -> -9 (only in wave 4)
  # -996: No parent in household -> -1 (only in wave 4)
  
  # Create a mapping based on source variable name
  if (source_name == "w4empsmum" || source_name == "w4empsdad") {
    x <- case_when(
      x == -999 ~ -2,
      x == -996 ~ -1,
      x == -99 ~ -3,
      x == -98 ~ -3,
      x == -94 ~ -8,
      x == -92 ~ -9,
      TRUE ~ as.numeric(x)
    )
  } else {
    x <- case_when(
      x == -999 ~ -2,
      x == -99 ~ -3,
      x == -98 ~ -3,
      x == -94 ~ -8,
      TRUE ~ as.numeric(x)
    )
  }
  return(x)
}

# Create the 8 time-varying parental economic activity variables
# Mother variables
df_merged$ecoactma14 <- clean_ecoact(df_merged$W1empsmum, "W1empsmum")
df_merged$ecoactma15 <- clean_ecoact(df_merged$W2empsmum, "W2empsmum")
df_merged$ecoactma16 <- clean_ecoact(df_merged$W3empsmum, "W3empsmum")
df_merged$ecoactma17 <- clean_ecoact(df_merged$w4empsmum, "w4empsmum")

# Father variables
df_merged$ecoactpa14 <- clean_ecoact(df_merged$W1empsdad, "W1empsdad")
df_merged$ecoactpa15 <- clean_ecoact(df_merged$W2empsdad, "W2empsdad")
df_merged$ecoactpa16 <- clean_ecoact(df_merged$W3empsdad, "W3empsdad")
df_merged$ecoactpa17 <- clean_ecoact(df_merged$w4empsdad, "w4empsdad")

# Keep only NSID and the 8 derived variables
output <- df_merged %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write output to CSV
write_csv(output, "data/output/cleaned_data.csv")

# Print summary
print(paste("Output dimensions:", nrow(output), "rows,", ncol(output), "columns"))
print(head(output))
