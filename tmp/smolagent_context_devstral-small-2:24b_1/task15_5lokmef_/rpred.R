library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load the datasets
wave8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge the datasets using full_join by NSID
merged_data <- full_join(wave8, wave9, by = "NSID")

# Select and rename the income variables
cleaned_data <- merged_data %>%
  select(NSID, W8DINCB, W9DINCB) %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB)

# Standard missing value codes
standard_missing_codes <- list(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked/interviewed",
  "-2" = "Script error/lost"
)

# Recode wave-specific negative codes to the standard missing scheme
cleaned_data <- cleaned_data %>%
  mutate(
    inc25 = case_when(
      inc25 == -1 ~ -1,
      inc25 == -2 ~ -2,
      inc25 == -3 ~ -3,
      inc25 == -8 ~ -8,
      inc25 == -9 ~ -9,
      TRUE ~ inc25
    ),
    inc32 = case_when(
      inc32 == -1 ~ -1,
      inc32 == -2 ~ -2,
      inc32 == -3 ~ -3,
      inc32 == -8 ~ -8,
      inc32 == -9 ~ -9,
      TRUE ~ inc32
    )
  )

# Convert each income variable to a labelled factor with explicit labels
inc25_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked/interviewed",
  "-2" = "Script error/lost",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

inc32_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Item not applicable",
  "-3" = "Not asked/interviewed",
  "-2" = "Script error/lost",
  "1" = "less than 25",
  "2" = "25 to 50",
  "3" = "50 to 90",
  "4" = "90 to 140",
  "5" = "140 to 240",
  "6" = "240 to 300",
  "7" = "300 to 350",
  "8" = "350 to 400",
  "9" = "400 to 500",
  "10" = "500 to 600",
  "11" = "600 to 700",
  "12" = "700 to 800",
  "13" = "800 to 900",
  "14" = "900 to 1200",
  "15" = "1200 to 1400",
  "16" = "more than 1400"
)

cleaned_data <- cleaned_data %>%
  mutate(
    inc25 = factor(inc25, levels = names(inc25_labels), labels = inc25_labels),
    inc32 = factor(inc32, levels = names(inc32_labels), labels = inc32_labels)
  )

# Write the cleaned data to a CSV file
write.csv(cleaned_data, "data/output/cleaned_data.csv", row.names = FALSE)