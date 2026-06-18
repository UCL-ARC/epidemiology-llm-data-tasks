library(readr)
library(dplyr)

# Load all files
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Extract and clean IMD variables
# Wave 2 (Age 15)
imd15 <- wave2 %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd15 = case_when(
    IMDRSCORE == -94 ~ -8,
    is.na(IMDRSCORE) ~ -3,
    TRUE ~ IMDRSCORE
  )) %>%
  select(NSID, imd15)

# Wave 3 (Age 16)
imd16 <- wave3 %>%
  select(NSID, IMDRSCORE) %>%
  mutate(imd16 = case_when(
    IMDRSCORE == -94 ~ -8,
    is.na(IMDRSCORE) ~ -3,
    TRUE ~ IMDRSCORE
  )) %>%
  select(NSID, imd16)

# Wave 9 (Age 32)
imd32 <- wave9 %>%
  select(NSID, W9DIMDD) %>%
  mutate(imd32 = case_when(
    W9DIMDD == -8 ~ -8,
    is.na(W9DIMDD) ~ -3,
    TRUE ~ W9DIMDD
  )) %>%
  select(NSID, imd32)

# Merge all IMD variables
result <- full_join(imd15, imd16, by = "NSID") %>%
  full_join(., imd32, by = "NSID")

# Write output
dir.create("data/output", showWarnings = FALSE)
write_csv(result, "data/output/cleaned_data.csv")

cat("Output written successfully.\n")
cat("Number of rows:", nrow(result), "\n")
cat("Variables:", names(result), "\n")

# Verify
result2 <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
for (var in c("imd15", "imd16", "imd32")) {
  cat(sprintf("\n%s summary:\n", var))
  print(summary(result2[[var]]))
}
