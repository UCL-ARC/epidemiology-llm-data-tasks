library(dplyr)
library(tidyr)
library(readr)

# Function to map source missing codes to standard missing codes according to the additional requirements
map_missing <- function(x) {
  # Convert R NA to standard missing code -3 first
  x <- ifelse(is.na(x), -3, x)
  # Map other source codes based on label meanings
  # Source -94 -> -2, -99 -> -3, -92 -> -9, -91 -> -1, -1 (Don’t know) -> -8
  # Script errors etc (-999,-998,-997,-995) -> -2
  x[x %in% c(-999, -998, -997, -995, -94)] <- -2
  x[x %in% c(-99)] <- -3
  x[x %in% c(-92)] <- -9
  x[x %in% c(-91)] <- -1
  x[x %in% c(-1)] <- -8
  return(x)
}

# Load the four survey files
file1 <- "data/input/wave_one_lsype_young_person_2020.tab"
file2 <- "data/input/wave_two_lsype_young_person_2020.tab"
file3 <- "data/input/wave_three_lsype_family_background_2020.tab"
file4 <- "data/input/wave_four_lsype_family_background_2020.tab"

# Read each file (force all columns to character first, will cast numeric later)
raw1 <- read_delim(file1, delim = "\t", col_types = cols(.default = "c"))
raw2 <- read_delim(file2, delim = "\t", col_types = cols(.default = "c"))
raw3 <- read_delim(file3, delim = "\t", col_types = cols(.default = "c"))
raw4 <- read_delim(file4, delim = "\t", col_types = cols(.default = "c"))

# Convert the relevant language variables to numeric and map missing codes
raw1 <- raw1 %>% mutate(across(c(W1englangYP), ~ as.numeric(map_missing(.x))))
raw2 <- raw2 %>% mutate(across(c(W2EnglangYP), ~ as.numeric(map_missing(.x))))
raw3 <- raw3 %>% mutate(across(c(W3englangHH), ~ as.numeric(map_missing(.x))))
raw4 <- raw4 %>% mutate(across(c(W4EngLangHH), ~ as.numeric(map_missing(.x))))

# Merge the data frames by the cohort identifier NSID
merged <- raw1 %>%
  full_join(raw2, by = "NSID") %>%
  full_join(raw3, by = "NSID") %>%
  full_join(raw4, by = "NSID")

# Derive the consolidated language variable (lang)
merged <- merged %>%
  mutate(lang = case_when(
    !is.na(W1englangYP) & W1englangYP %in% 1:4 ~ W1englangYP,
    !is.na(W2EnglangYP) & W2EnglangYP %in% 1:4 ~ W2EnglangYP,
    !is.na(W3englangHH) & W3englangHH %in% 1:4 ~ W3englangHH,
    !is.na(W4EngLangHH) & W4EngLangHH %in% 1:4 ~ W4EngLangHH,
    TRUE ~ NA_real_
  ))

# Replace R NA with standard missing code -3
merged$lang[is.na(merged$lang)] <- -3

# Keep only the required final variables
final_df <- merged %>% select(NSID, lang)

# Write the cleaned data to CSV
write_csv(final_df, "data/output/cleaned_data.csv")

cat("Cleaning complete. Output written to data/output/cleaned_data.csv\n")