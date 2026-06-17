library(haven)
library(dplyr)
library(readr)

# Load each wave file
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim="\t", col_types = cols())
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim="\t", col_types = cols())
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim="\t", col_types = cols())
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim="\t", col_types = cols())

# Merge all waves by NSID preserving all cases
merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID")

# Function to map missing codes to standard scheme
map_missing <- function(x) {
  y <- x
  y[is.na(y)] <- -3
  y[y == -92] <- -9
  y[y == -94] <- -8
  y[y == -99] <- -3
  y[y %in% c(-999, -998, -997, -995, -996, -98)] <- -2
  return(y)
}

# Labels for employment categories and missing codes
emp_labels <- c(
  "1" = "Doing paid work for 30 or more hours a week",
  "2" = "Doing paid work for fewer than 30 hours a week",
  "3" = "Unemployed/ Looking for a job",
  "4" = "On a training course or scheme",
  "5" = "In full-time education/ at school",
  "6" = "Looking after the family/ household",
  "7" = "Retired from work altogether",
  "8" = "Sick/ disabled",
  "9" = "Other"
)
missing_labels <- c(
  "-9" = "Refusal",
  "-8" = "Don\'t know / insufficient information",
  "-7" = "Prefer not to say",
  "-3" = "Not asked at fieldwork stage / not interviewed",
  "-2" = "Schedule not applicable / script error / information lost",
  "-1" = "Item not applicable"
)
all_labels <- c(emp_labels, missing_labels)

# Harmonise parental economic activity variables
merged <- merged %>%
  mutate(
    ecoactma14 = as_factor(labelled(as.character(map_missing(W1empsmum)), labels = all_labels)),
    ecoactpa14 = as_factor(labelled(as.character(map_missing(W1empsdad)), labels = all_labels)),
    ecoactma15 = as_factor(labelled(as.character(map_missing(W2empsmum)), labels = all_labels)),
    ecoactpa15 = as_factor(labelled(as.character(map_missing(W2empsdad)), labels = all_labels)),
    ecoactma16 = as_factor(labelled(as.character(map_missing(W3empsmum)), labels = all_labels)),
    ecoactpa16 = as_factor(labelled(as.character(map_missing(W3empsdad)), labels = all_labels)),
    ecoactma17 = as_factor(labelled(as.character(map_missing(w4empsmum)), labels = all_labels)),
    ecoactpa17 = as_factor(labelled(as.character(map_missing(w4empsdad)), labels = all_labels))
  )

# Select final variables
final_df <- merged %>%
  select(NSID,
         ecoactma14, ecoactpa14,
         ecoactma15, ecoactpa15,
         ecoactma16, ecoactpa16,
         ecoactma17, ecoactpa17)

# Write to CSV
write_csv(final_df, "data/output/cleaned_data.csv")