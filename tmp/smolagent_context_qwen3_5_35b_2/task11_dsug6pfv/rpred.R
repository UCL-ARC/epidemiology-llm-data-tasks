library(readr)
library(dplyr)

# Load the four wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")

# Merge all waves by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave3, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")

# Define a recoding function
code_ecoact <- function(x) {
  # Map substantive codes 1-9 to themselves
  # Map missing codes according to additional requirements
  recoded <- case_when(
    x %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9) ~ as.integer(x),
    x %in% c(-99, -98, -996) ~ -3,
    x == -999 ~ -2,
    x == -94 ~ -8,
    TRUE ~ NA_real_
  )
  return(recoded)
}

# Create the 8 output variables
merged$ecoactma14 <- code_ecoact(merged$W1empsmum)
merged$ecoactpa14 <- code_ecoact(merged$W1empsdad)
merged$ecoactma15 <- code_ecoact(merged$W2empsmum)
merged$ecoactpa15 <- code_ecoact(merged$W2empsdad)
merged$ecoactma16 <- code_ecoact(merged$W3empsmum)
merged$ecoactpa16 <- code_ecoact(merged$W3empsdad)
merged$ecoactma17 <- code_ecoact(merged$w4empsmum)
merged$ecoactpa17 <- code_ecoact(merged$w4empsdad)

# Keep only NSID and the 8 output variables
output <- merged %>%
  select(NSID, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to CSV
write_csv(output, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv\n")
