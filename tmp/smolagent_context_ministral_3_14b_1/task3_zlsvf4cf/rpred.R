# Load required libraries
library(readr)
library(dplyr)

# Load data files
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave3 <- readr::read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- readr::read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Ensure numeric conversion and select only needed columns
wave1 <- wave1 %>% select(NSID, W1englangYP = W1englangYP) %>% mutate(W1englangYP = as.numeric(W1englangYP))
wave2 <- wave2 %>% select(NSID, W2EnglangYP = W2EnglangYP) %>% mutate(W2EnglangYP = as.numeric(W2EnglangYP))
wave3 <- wave3 %>% select(NSID, W3englangHH = W3englangHH) %>% mutate(W3englangHH = as.numeric(W3englangHH))
wave4 <- wave4 %>% select(NSID, W4EngLangHH = W4EngLangHH) %>% mutate(W4EngLangHH = as.numeric(W4EngLangHH))

# Merge datasets
merged_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Create lang variable with earliest valid response and proper missing value handling
merged_data <- merged_data %>%
  mutate(lang = case_when(
    !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
    !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
    !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
    !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
    TRUE ~ -3
  )) %>%
  mutate(lang = case_when(
    lang == -94 ~ -2,
    lang == -1 ~ -8,
    lang == -999 | lang == -998 | lang == -997 | lang == -995 ~ -2,
    lang == -92 ~ -9,
    lang == -91 ~ -1,
    TRUE ~ lang
  ))

# Select only required columns
cleaned_data <- merged_data %>% select(NSID, lang)

# Create output directory if needed
if (!dir.exists("data/output")) dir.create("data/output")

# Write output file
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Verify output file was created and contains data
output_file <- "data/output/cleaned_data.csv"
file_exists <- file.exists(output_file)
file_size <- ifelse(file_exists, file.info(output_file)$size, 0)

# Print verification message
message(paste("Output file verification:", 
              "File exists:", file_exists, 
              "File size (bytes):", file_size))

# Show sample of the data
message("Sample of cleaned data:")
print(head(cleaned_data))