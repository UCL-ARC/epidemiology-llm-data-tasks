# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)

# Define file paths
base_path <- "data/input/"

# Helper to read a tab-delimited file
read_tab <- function(fname){
  read_delim(paste0(base_path, fname), delim = "\t", col_types = cols(), progress = FALSE)
}

# Read files
wave1 <- read_tab("wave_one_lsype_young_person_2020.tab")
wave4 <- read_tab("wave_four_lsype_young_person_2020.tab")
wave2 <- read_tab("wave_two_lsype_family_background_2020.tab")
wave3 <- read_tab("wave_three_lsype_family_background_2020.tab")
wave8 <- read_tab("ns8_2015_derived.tab")
wave9_derived <- read_tab("ns9_2022_derived_variables.tab")
wave9_main <- read_tab("ns9_2022_main_interview.tab")

# Rename wave2 and wave3 variables to include age
wave2_sel <- wave2 %>% select(NSID, urbind15 = urbind, gor15 = gor)
wave3_sel <- wave3 %>% select(NSID, urbind16 = urbind, gor16 = gor)

# Merge wave2 & wave3
wf23 <- full_join(wave2_sel, wave3_sel, by = "NSID")

# Merge all waves into one dataframe
merged <- reduce(list(wave1, wave4, wf23, wave8, wave9_derived, wave9_main), full_join, by = "NSID")

# Mapping functions with missing handling
map_urbind <- function(x){
  y <- dplyr::recode(x, `-94.0` = -8, `-9.0` = -9, `-8.0` = -8, `-1.0` = -1, .default = x)
  y[is.na(y)] <- -3
  y
}

map_gor <- function(x){
  y <- dplyr::recode(x, `-94.0` = -8, `-9.0` = -9, `-8.0` = -8, `-1.0` = -1, .default = x)
  y[is.na(y)] <- -3
  y
}

map_reg_or <- function(x){
  y <- dplyr::recode(x, `-9.0` = -9, `-8.0` = -8, `-1.0` = -1, .default = x)
  y[is.na(y)] <- -3
  y
}

map_nation <- function(x){
  y <- dplyr::recode(x, `-9.0` = -9, `-8.0` = -8, `-3.0` = -3, `-1.0` = -1, .default = x)
  y[is.na(y)] <- -3
  y
}

# Create cleaned variables
cleaned <- merged %>%
  mutate(
    regub15 = map_urbind(urbind15),
    regub16 = map_urbind(urbind16),
    regov15 = map_gor(gor15),
    regov16 = map_gor(gor16),
    regor25 = map_reg_or(W8DGOR),
    regor32 = map_reg_or(W9DRGN),
    regint32 = map_nation(W9NATIONRES)
  ) %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Define value labels
urbind_labels <- c(
  "1" = "Urban >= 10k - sparse",
  "2" = "Town & Fringe - sparse",
  "3" = "Village - sparse",
  "4" = "Hamlet and Isolated Dwelling - sparse",
  "5" = "Urban >= 10k - less sparse",
  "6" = "Town & Fringe - less sparse",
  "7" = "Village - less sparse",
  "8" = "Hamlet & Isolated Dwelling"
)

gor_labels <- c(
  "1" = "North East",
  "2" = "North West",
  "3" = "Yorkshire and The Humber",
  "4" = "East Midlands",
  "5" = "West Midlands",
  "6" = "East of England",
  "7" = "London",
  "8" = "South East",
  "9" = "South West"
)

region_labels <- c(
  "1" = "North East",
  "2" = "North West",
  "3" = "Yorkshire and the Humber",
  "4" = "East Midlands",
  "5" = "West Midlands",
  "6" = "East of England",
  "7" = "London",
  "8" = "South East",
  "9" = "South West",
  "10" = "Wales",
  "11" = "Scotland",
  "12" = "Northern Ireland",
  "13" = "Unknown due to faulty/missing postcode"
)

nation_labels <- c(
  "1" = "England",
  "2" = "Scotland",
  "3" = "Wales",
  "4" = "Northern Ireland",
  "5" = "Outside of UK or unknown"
)

# Apply factor conversion with labels
cleaned <- cleaned %>%
  mutate(
    regub15 = factor(regub15, levels = as.numeric(names(urbind_labels)), labels = urbind_labels, ordered = FALSE),
    regub16 = factor(regub16, levels = as.numeric(names(urbind_labels)), labels = urbind_labels, ordered = FALSE),
    regov15 = factor(regov15, levels = as.numeric(names(gor_labels)), labels = gor_labels, ordered = FALSE),
    regov16 = factor(regov16, levels = as.numeric(names(gor_labels)), labels = gor_labels, ordered = FALSE),
    regor25 = factor(regor25, levels = as.numeric(names(region_labels)), labels = region_labels, ordered = FALSE),
    regor32 = factor(regor32, levels = as.numeric(names(region_labels)), labels = region_labels, ordered = FALSE),
    regint32 = factor(regint32, levels = as.numeric(names(nation_labels)), labels = nation_labels, ordered = FALSE)
  )

# Write output
output_path <- "data/output/"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
write_csv(cleaned, file.path(output_path, "cleaned_data.csv"))
