library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(haven)

# Load all files
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Full join all files by NSID
merged <- full_join(w6, w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Create sori variables using mutate and case_when
output <- merged %>%
  mutate(
    # Map W6SexualityYP to sori19 (Wave 6 = Age 19)
    sori19 = case_when(
      W6SexualityYP == -97 ~ -2L,
      W6SexualityYP == -92 ~ -9L,
      W6SexualityYP == -91 ~ -1L,
      W6SexualityYP == -1 ~ -8L,
      TRUE ~ as.integer(W6SexualityYP)
    ),
    # Map W7SexualityYP to sori20 (Wave 7 = Age 20)
    sori20 = case_when(
      W7SexualityYP == -100 ~ -2L,
      W7SexualityYP == -97 ~ -2L,
      W7SexualityYP == -92 ~ -9L,
      W7SexualityYP == -91 ~ -1L,
      W7SexualityYP == -1 ~ -8L,
      TRUE ~ as.integer(W7SexualityYP)
    ),
    # Map W8SEXUALITY to sori25 (Wave 8 = Age 25)
    sori25 = case_when(
      W8SEXUALITY == -9 ~ -9L,
      W8SEXUALITY == -8 ~ -8L,
      W8SEXUALITY == -1 ~ -1L,
      TRUE ~ as.integer(W8SEXUALITY)
    ),
    # Map W9SORI to sori32 (Wave 9 = Age 32)
    sori32 = case_when(
      W9SORI == -9 ~ -9L,
      W9SORI == -8 ~ -8L,
      W9SORI == -3 ~ -3L,
      W9SORI == -1 ~ -1L,
      W9SORI == 5 ~ -7L,
      TRUE ~ as.integer(W9SORI)
    )
  ) %>%
  select(NSID, sori19, sori20, sori25, sori32)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Successfully created cleaned_data.csv\n")
cat("Rows:", nrow(output), "\n")
cat("Columns:", ncol(output), "\n")
