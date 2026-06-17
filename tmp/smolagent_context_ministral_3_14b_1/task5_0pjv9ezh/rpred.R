
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Load datasets from metadata
wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
ns8 <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
ns9 <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Ensure NSID is a character variable in all datasets
wave6$NSID <- as.character(wave6$NSID)
ns8$NSID <- as.character(ns8$NSID)
ns9$NSID <- as.character(ns9$NSID)

# Function to map missing values
map_missing <- function(x) {
  if (is.numeric(x)) {
    x <- as.character(x)
  }
  recode_values <- c(
    "-997.0" = -2, "-97.0" = -7, "-92.0" = -9, "-91.0" = -1, "-1.0" = -8,
    "-9.0" = -9, "-8.0" = -8
  )
  if (length(x) == 0) return(-3)
  as.numeric(recode_values[match(x, names(recode_values))])
}

# Create mapping for partnership categories
map_partnr <- function(x, wave) {
  if (!is.numeric(x)) return(NA_integer_)

  if (wave == "wave6") {
    c(1, 2, 3, 4, 5)[x]
  } else if (wave == "ns8") {
    c(1, 2, 3, 4, 5, 2, 3, 4, 5)[x]
  } else if (wave == "ns9") {
    c(1, 2, 4, 3, 5, 2, 4, 5)[x]
  }
}

# Create derived variables for each dataset separately
wave6_partnr <- wave6 %>%
  mutate(
    W6MarStatYP_clean = map_missing(W6MarStatYP),
    partnr19 = map_partnr(W6MarStatYP_clean, "wave6"),
    partnr19 = ifelse(is.na(partnr19), -3, partnr19)
  )

ns8_partnr <- ns8 %>%
  mutate(
    W8DMARSTAT_clean = map_missing(W8DMARSTAT),
    partnradu25 = W8DMARSTAT_clean,
    partnr25 = map_partnr(W8DMARSTAT_clean, "ns8"),
    partnr25 = ifelse(is.na(partnr25), -3, partnr25)
  )

ns9_partnr <- ns9 %>%
  mutate(
    W9DMARSTAT_clean = map_missing(W9DMARSTAT),
    partnradu32 = W9DMARSTAT_clean,
    partnr32 = map_partnr(W9DMARSTAT_clean, "ns9"),
    partnr32 = ifelse(is.na(partnr32), -3, partnr32)
  )

# Merge datasets step by step
merged_data <- wave6 %>%
  left_join(ns8, by = "NSID") %>%
  left_join(ns9, by = "NSID")

# Add derived variables
final_data <- merged_data %>%
  left_join(wave6_partnr %>% select(NSID, partnr19), by = "NSID") %>%
  left_join(ns8_partnr %>% select(NSID, partnradu25, partnr25), by = "NSID") %>%
  left_join(ns9_partnr %>% select(NSID, partnradu32, partnr32), by = "NSID") %>%
  select(NSID, partnr19, partnradu25, partnr25, partnradu32, partnr32)

# Define factor levels and labels
final_data <- final_data %>%
  mutate(
    partnr19 = factor(partnr19,
                      levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5),
                      labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "Single", "Married", "Separated", "Divorced", "Widowed")),
    partnr25 = factor(partnr25,
                      levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5),
                      labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "Single", "Married", "Separated", "Divorced", "Widowed")),
    partnr32 = factor(partnr32,
                      levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5),
                      labels = c("Refusal", "Don't know", "Prefer not to say", "Not asked", "Schedule not applicable", "Not applicable", "Single", "Married", "Separated", "Divorced", "Widowed")),
    partnradu25 = factor(partnradu25,
                        levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("Refusal", "Don't know", "Insufficient information", "Not asked", "Schedule not applicable", "Not applicable", "Single", "Married", "Separated", "Divorced", "Widowed", "Civil Partner", "Separated but still legally in a CP", "Former Civil Partner", "Surviving Civil Partner")),
    partnradu32 = factor(partnradu32,
                        levels = c(-9, -8, -7, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c("Refusal", "Don't know", "Insufficient information", "Not asked", "Schedule not applicable", "Not applicable", "Single", "Married", "Divorced", "Legally separated", "Widowed", "Civil Partner", "Former Civil Partner", "Surviving Civil Partner"))
  )

# Ensure output directory exists and write output
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

readr::write_csv(final_data, "data/output/cleaned_data.csv")
