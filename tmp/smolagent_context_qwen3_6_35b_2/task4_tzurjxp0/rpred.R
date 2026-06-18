library(dplyr)
library(readr)
library(labelled)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Function to recode a source variable to standard missing codes
recode_sori <- function(x, source_name) {
  result <- x
  
  # Map -97 to -9 (Refusal/declined)
  result[result == -97] <- -9
  
  # Map -100 to -9 (Refusal/declined)
  result[result == -100] <- -9
  
  # Map -92 to -9 (Refused)
  result[result == -92] <- -9
  
  # Map -91 to -1 (Not applicable)
  result[result == -91] <- -1
  
  # Map -9 to -9 (Refused) - already correct
  
  # Map -8 to -8 (Don't know) - already correct
  
  # Map -3 to -3 (Not asked) - already correct
  
  # Handle -1 based on source variable meaning
  # In W6 and W7, -1 means "Don't know" -> map to -8
  if (source_name %in% c("W6SexualityYP", "W7SexualityYP")) {
    result[result == -1] <- -8
  }
  # In W8 and W9, -1 means "Not applicable" -> keep as -1
  
  # Convert remaining NAs to -3 (Not asked)
  result[is.na(result)] <- -3
  
  return(result)
}

# Create sori19 from W6SexualityYP (Wave 6, Age 19)
sori19_df <- w6 %>%
  mutate(sori19 = recode_sori(W6SexualityYP, "W6SexualityYP")) %>%
  select(NSID, sori19)

# Create sori20 from W7SexualityYP (Wave 7, Age 20)
sori20_df <- w7 %>%
  mutate(sori20 = recode_sori(W7SexualityYP, "W7SexualityYP")) %>%
  select(NSID, sori20)

# Create sori25 from W8SEXUALITY (Wave 8, Age 25)
sori25_df <- w8 %>%
  mutate(sori25 = recode_sori(W8SEXUALITY, "W8SEXUALITY")) %>%
  select(NSID, sori25)

# Create sori32 from W9SORI (Wave 9, Age 32)
sori32_df <- w9 %>%
  mutate(sori32 = W9SORI) %>%
  mutate(
    # Map valid categories (1-4 stay the same)
    sori32 = case_when(
      sori32 == -1 ~ -1,        # Not applicable
      sori32 == -3 ~ -3,        # Not asked at fieldwork stage
      sori32 == -8 ~ -8,        # Don't know
      sori32 == -9 ~ -9,        # Refused
      sori32 == 5 ~ -7,         # Prefer not to say -> -7
      TRUE ~ sori32             # Valid categories 1-4
    )
  ) %>%
  select(NSID, sori32)

# Merge all datasets by NSID using full_join
df_clean <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(sori19_df, by = "NSID") %>%
  full_join(sori20_df, by = "NSID") %>%
  full_join(sori25_df, by = "NSID") %>%
  full_join(sori32_df, by = "NSID") %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write to CSV
write_csv(df_clean, "data/output/cleaned_data.csv")

# Print summary
cat("Cleaned data written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(df_clean), "\n")
cat("\nSummary of sori19:\n")
print(table(df_clean$sori19, useNA = "ifany"))
cat("\nSummary of sori20:\n")
print(table(df_clean$sori20, useNA = "ifany"))
cat("\nSummary of sori25:\n")
print(table(df_clean$sori25, useNA = "ifany"))
cat("\nSummary of sori32:\n")
print(table(df_clean$sori32, useNA = "ifany"))
