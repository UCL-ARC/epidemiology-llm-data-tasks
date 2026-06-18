options(repos = c(CRAN = "https://cloud.r-project.org/"))
list_of_packages <- c('dplyr', 'purrr', 'readr')
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
})

# Data path
data_path <- 'data/input/'

S1 <- read_delim(file.path(data_path, "wave_one_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma14_raw = W1empsmum, ecoactdtpa14_raw = W1empsdad)

S2 <- read_delim(file.path(data_path, "wave_two_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma15_raw = W2empsmum, ecoactdtpa15_raw = W2empsdad)

S3 <- read_delim(file.path(data_path, "wave_three_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma16_raw = W3empsmum, ecoactdtpa16_raw = W3empsdad)

S4 <- read_delim(file.path(data_path, "wave_four_lsype_family_background_2020.tab"), show_col_types = FALSE) %>%
  select(NSID, ecoactdtma17_raw = w4empsmum, ecoactdtpa17_raw = w4empsdad)

ecoactDT_parents_all <- reduce(list(S1, S2, S3, S4), full_join, by = "NSID")

# Label definitions
ecoact_levels <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, -1L, -2L, -3L, -5L, -7L, -8L, -9L)
ecoact_labels <- c(
  "FT paid work", "PT paid work", "Unemployed", "Training", "Education",
  "Looking after home/family", "Retired from work altogether", "Sick/disabled", "Other",
  "Item not applicable", "Script error/information lost",
  "Not asked at the fieldwork stage/did not participate at specific wave/was not surveyed",
  "Data not available", "Prefer not to say", "Don't know/insufficient information", "Refusal"
)

# Recode helper
recode_ecoactDT <- function(x) {
  case_when(
    x %in% 1:9  ~ x,
    x == -94     ~ -8,
    x == -92     ~ -9,
    x == -999    ~ -2,
    x %in% c(-996, -98, -99) ~ -3,
    is.na(x)     ~ -3,
    TRUE         ~ NA_real_
  )
}

# Apply recode to each sweep
ecoactDT_parents_rec <- ecoactDT_parents_all %>%
  mutate(
    ecoactdtma14 = recode_ecoactDT(ecoactdtma14_raw),
    ecoactdtpa14 = recode_ecoactDT(ecoactdtpa14_raw),
    ecoactdtma15 = recode_ecoactDT(ecoactdtma15_raw),
    ecoactdtpa15 = recode_ecoactDT(ecoactdtpa15_raw),
    ecoactdtma16 = recode_ecoactDT(ecoactdtma16_raw),
    ecoactdtpa16 = recode_ecoactDT(ecoactdtpa16_raw),
    ecoactdtma17 = recode_ecoactDT(ecoactdtma17_raw),
    ecoactdtpa17 = recode_ecoactDT(ecoactdtpa17_raw)
  )

# Extract final variables, convert to factors, and write
ecoactDT_parents_all <- ecoactDT_parents_rec %>%
  select(NSID, matches("^ecoactdt(ma|pa)\\d+$")) %>%
  mutate(
    across(
      -NSID,
      ~ factor(.x, levels = ecoact_levels, labels = ecoact_labels)
    )
  )

# Create output directory if it doesn't exist
dir.create(file.path(getwd(), "data", "output"), recursive = TRUE, showWarnings = FALSE)

output_data_path <- file.path(getwd(), "data", "output", "output.csv")
write.csv(ecoactDT_parents_all, output_data_path, row.names = FALSE)