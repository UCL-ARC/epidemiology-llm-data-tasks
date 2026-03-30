library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Step 1: Load datasets
wave8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Step 2: Merge datasets by NSID
merged_data <- full_join(wave8, wave9, by = "NSID")

# Step 3: Select and rename income variables
merged_data <- merged_data %>%
  rename(inc25 = W8DINCB, inc32 = W9DINCB) %>%
  select(NSID, inc25, inc32)

# Step 4: Define labels for income variables
inc_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-1" = "Not applicable",
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

# Step 5: Recode missing values for inc25 (Wave 8)
merged_data <- merged_data %>%
  mutate(inc25 = ifelse(is.na(inc25), -3, inc25)) %>%
  mutate(inc25 = as.numeric(as.character(inc25))) %>%
  mutate(inc25 = factor(inc25, levels = as.numeric(names(inc_labels)), labels = inc_labels))

# Step 6: Recode missing values for inc32 (Wave 9)
merged_data <- merged_data %>%
  mutate(inc32 = ifelse(is.na(inc32), -3, inc32)) %>%
  mutate(inc32 = as.numeric(as.character(inc32))) %>%
  mutate(inc32 = factor(inc32, levels = as.numeric(names(inc_labels)), labels = inc_labels))

# Step 7: Output the cleaned dataset
write.csv(merged_data, "data/output/cleaned_data.csv", row.names = FALSE)