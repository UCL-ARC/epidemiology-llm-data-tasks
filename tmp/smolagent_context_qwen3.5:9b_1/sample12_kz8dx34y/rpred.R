library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

files <- list(
  wave1 = "data/input/wave_one_lsype_young_person_2020.tab",
  wave4 = "data/input/wave_four_lsype_young_person_2020.tab",
  wave5 = "data/input/wave_five_lsype_young_person_2020.tab",
  wave6 = "data/input/wave_six_lsype_young_person_2020.tab",
  wave7 = "data/input/wave_seven_lsype_young_person_2020.tab",
  wave8 = "data/input/ns8_2015_derived.tab",
  wave9 = "data/input/ns9_2022_main_interview.tab"
)

# Load all files
wave1 <- read_delim(files$wave1, delim = "\t")
wave4 <- read_delim(files$wave4, delim = "\t")
wave5 <- read_delim(files$wave5, delim = "\t")
wave6 <- read_delim(files$wave6, delim = "\t")
wave7 <- read_delim(files$wave7, delim = "\t")
wave8 <- read_delim(files$wave8, delim = "\t")
wave9 <- read_delim(files$wave9, delim = "\t")

nssec_labels <- c(
  "Employers in large organisations",
  "Higher managerial and administrative occupations",
  "Higher professional occupations",
  "Lower professional and higher technical occupations",
  "Lower managerial and administrative occupations",
  "Higher supervisory occupations",
  "Intermediate occupations",
  "Employers in small establishments",
  "Own account workers",
  "Lower supervisory occupations",
  "Lower technical occupations",
  "Semi-routine occupations",
  "Routine occupations",
  "Never worked and Long-term unemployed",
  "Full-time students",
  "Occupations not stated or inadequately described",
  "Not classifiable for other reasons"
)

# Function to collapse fractional NS-SEC codes
collapse_nssec <- function(x) {
  x <- as.numeric(x)
  x <- floor(x)
  return(x)
}

# Wave 4 (Age 17): W4nsseccatYP
# Missing: -99 (YP Not interviewed) -> -3, -91 (Not applicable) -> -1
wave4 <- wave4 %>%
  mutate(
    nssec17_raw = W4nsseccatYP,
    nssec17 = replace(as.integer(W4nsseccatYP), W4nsseccatYP == -99.0, -3),
    nssec17 = replace(nssec17, W4nsseccatYP == -91.0, -1),
    nssec17 = collapse_nssec(nssec17)
  )

# Wave 5 (Age 18): W5nsseccatYP
# Missing: -91 (Not applicable) -> -1, -999 and below -> -3
wave5 <- wave5 %>%
  mutate(
    nssec18_raw = W5nsseccatYP,
    nssec18 = replace(as.integer(W5nsseccatYP), W5nsseccatYP == -91.0, -1),
    nssec18 = replace(nssec18, W5nsseccatYP <= -92.0 & W5nsseccatYP >= -999.0, -3),
    nssec18 = collapse_nssec(nssec18)
  )

# Wave 6 (Age 19): w6nsseccatYP
# Missing: -91 (Not applicable) -> -1, -999 and below -> -3
wave6 <- wave6 %>%
  mutate(
    nssec19_raw = w6nsseccatYP,
    nssec19 = replace(as.integer(w6nsseccatYP), w6nsseccatYP == -91.0, -1),
    nssec19 = replace(nssec19, w6nsseccatYP <= -92.0 & w6nsseccatYP >= -999.0, -3),
    nssec19 = collapse_nssec(nssec19)
  )

# Wave 7 (Age 20): W7NSSECCat
# Missing: -91 (Not applicable) -> -1, -999 and below -> -3
wave7 <- wave7 %>%
  mutate(
    nssec20_raw = W7NSSECCat,
    nssec20 = replace(as.integer(W7NSSECCat), W7NSSECCat == -91.0, -1),
    nssec20 = replace(nssec20, W7NSSECCat <= -92.0 & W7NSSECCat >= -999.0, -3),
    nssec20 = collapse_nssec(nssec20)
  )

# Wave 8 (Age 25): W8DNSSEC17
# Missing: -9 (Refused) -> -9, -8 (Insufficient) -> -8, -1 (Not applicable) -> -1
wave8 <- wave8 %>%
  mutate(
    nssec25_raw = W8DNSSEC17,
    nssec25 = replace(as.integer(W8DNSSEC17), W8DNSSEC17 == -9.0, -9),
    nssec25 = replace(nssec25, W8DNSSEC17 == -8.0, -8),
    nssec25 = replace(nssec25, W8DNSSEC17 == -1.0, -1),
    nssec25 = collapse_nssec(nssec25)
  )

# Wave 9 (Age 32): W9NSSEC
# Missing: all standard codes
wave9 <- wave9 %>%
  mutate(
    nssec32_raw = W9NSSEC,
    nssec32 = replace(as.integer(W9NSSEC), W9NSSEC == -9.0, -9),
    nssec32 = replace(nssec32, W9NSSEC == -8.0, -8),
    nssec32 = replace(nssec32, W9NSSEC == -7.0, -7),
    nssec32 = replace(nssec32, W9NSSEC == -3.0, -3),
    nssec32 = replace(nssec32, W9NSSEC == -2.0, -2),
    nssec32 = replace(nssec32, W9NSSEC == -1.0, -1),
    nssec32 = collapse_nssec(nssec32)
  )

# Wave 1 has no NS-SEC variable, keep as is

# Create labelled factors for all nssec variables
wave4 <- wave4 %>%
  mutate(
    nssec17 = factor(nssec17, levels = c(-3, -1, 1:17), 
                     labels = c("Not asked", "Not applicable", nssec_labels),
                     ordered = FALSE)
  )

wave5 <- wave5 %>%
  mutate(
    nssec18 = factor(nssec18, levels = c(-3, -1, 1:17), 
                     labels = c("Not asked", "Not applicable", nssec_labels),
                     ordered = FALSE)
  )

wave6 <- wave6 %>%
  mutate(
    nssec19 = factor(nssec19, levels = c(-3, -1, 1:17), 
                     labels = c("Not asked", "Not applicable", nssec_labels),
                     ordered = FALSE)
  )

wave7 <- wave7 %>%
  mutate(
    nssec20 = factor(nssec20, levels = c(-3, -1, 1:17), 
                     labels = c("Not asked", "Not applicable", nssec_labels),
                     ordered = FALSE)
  )

wave8 <- wave8 %>%
  mutate(
    nssec25 = factor(nssec25, levels = c(-3, -1, 1:17), 
                     labels = c("Not asked", "Not applicable", nssec_labels),
                     ordered = FALSE)
  )

wave9 <- wave9 %>%
  mutate(
    nssec32 = factor(nssec32, levels = c(-3, -1, 1:17), 
                     labels = c("Not asked", "Not applicable", nssec_labels),
                     ordered = FALSE)
  )

# Merge all waves using full_join by NSID
cleaned_data <- full_join(wave1, wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Select only required variables
cleaned_data <- cleaned_data %>%
  select(NSID, nssec17, nssec18, nssec19, nssec20, nssec25, nssec32)

# Write to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Processing complete.\n")
cat("Total records:", nrow(cleaned_data), "\n")
cat("NSID range:", min(cleaned_data$NSID), "-", max(cleaned_data$NSID), "\n")