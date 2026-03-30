library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Define labels vector
labels_vec <- c(
  "-3" = "Not asked/interviewed",
  "-9" = "Refused",
  "-8" = "Don't know/insufficient information",
  "-1" = "Not applicable",
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background"
)

# Read Wave 1 (Age 14)
df1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df1 <- df1 %>% mutate(eth14 = case_when(W1ethnic2YP %in% c(-999, -94, -92, -91) ~ -3, W1ethnic2YP == -9 ~ -9, W1ethnic2YP == -8 ~ -8, W1ethnic2YP == -1 ~ -1, TRUE ~ W1ethnic2YP))
final_eth <- df1 %>% select(NSID, eth14)

# Read Wave 2 (Age 15)
df2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df2 <- df2 %>% mutate(eth15 = case_when(W2ethnicYP %in% c(-998, -997, -995, -99) ~ -3, W2ethnicYP == -92 ~ -9, W2ethnicYP == -91 ~ -1, W2ethnicYP == -9 ~ -9, W2ethnicYP == -8 ~ -8, W2ethnicYP == -1 ~ -1, TRUE ~ W2ethnicYP))
final_eth <- final_eth %>% full_join(df2 %>% select(NSID, eth15), by = "NSID")

# Read Wave 4 (Age 17)
df4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df4 <- df4 %>% mutate(eth17 = case_when(w4ethnic2YP %in% c(-999, -94) ~ -3, w4ethnic2YP == -91 ~ -1, w4ethnic2YP == -9 ~ -9, w4ethnic2YP == -8 ~ -8, w4ethnic2YP == -1 ~ -1, TRUE ~ w4ethnic2YP))
final_eth <- final_eth %>% full_join(df4 %>% select(NSID, eth17), by = "NSID")

# Read Wave 8 (Age 32)
df8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
df8 <- df8 %>% mutate(eth32_w8 = case_when(W8DETHN15 == -9 ~ -9, W8DETHN15 == -8 ~ -8, W8DETHN15 == -1 ~ -1, TRUE ~ W8DETHN15))
final_eth <- final_eth %>% full_join(df8 %>% select(NSID, eth32_w8), by = "NSID")

# Read Wave 9 (Age 32)
df9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
df9 <- df9 %>% mutate(eth32_w9 = case_when(W9DETHN15 == -8 ~ -8, W9DETHN15 == -9 ~ -9, W9DETHN15 == -1 ~ -1, TRUE ~ W9DETHN15))
final_eth <- final_eth %>% full_join(df9 %>% select(NSID, eth32_w9), by = "NSID")

# Create harmonized eth variable - prioritize earliest valid response
final_eth <- final_eth %>% mutate(eth = case_when(!is.na(eth14) & eth14 >= 1 & eth14 <= 16 ~ eth14, !is.na(eth15) & eth15 >= 1 & eth15 <= 16 ~ eth15, !is.na(eth17) & eth17 >= 1 & eth17 <= 16 ~ eth17, !is.na(eth32_w8) & eth32_w8 >= 1 & eth32_w8 <= 16 ~ eth32_w8, !is.na(eth32_w9) & eth32_w9 >= 1 & eth32_w9 <= 16 ~ eth32_w9, TRUE ~ -3))

# Convert to factor with labels
final_eth$eth <- factor(final_eth$eth, levels = c(-3, -9, -8, -1, 1:16), labels = as.character(labels_vec))

# Remove intermediate variables
final_eth <- final_eth %>% select(-eth14, -eth15, -eth17, -eth32_w8, -eth32_w9)

# Reorder columns
final_eth <- final_eth %>% select(NSID, eth)

# Write to output
write_csv(final_eth, "data/output/cleaned_data.csv")

print("Data cleaning complete!")
print(paste("Output rows:", nrow(final_eth)))
print(paste("Variables:", paste(names(final_eth), collapse = ", ")))
