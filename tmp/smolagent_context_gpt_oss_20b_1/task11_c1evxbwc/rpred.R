library(readr)
library(dplyr)

# Helper function to harmonise missing values
map_missing <- function(x){
  x <- ifelse(x == -999, -2, x)
  x <- ifelse(x == -94, -8, x)
  x <- ifelse(x == -92, -9, x)
  x <- ifelse(x == -91, -1, x)
  x <- ifelse(x %in% c(-99, -98, -996), -3, x)
  x[is.na(x)] <- -3
  return(x)
}

# Create output directory
if(!dir.exists("data/output")) dir.create("data/output", recursive = TRUE)

# Function to load and rename columns, converting to lower case
load_wave <- function(path, wave){
  df <- read_delim(path, delim = "\t", col_names = TRUE, trim_ws = TRUE)
  colnames(df) <- tolower(colnames(df))
  if(wave == 1) {
    df <- df %>% rename(ecoactma14 = w1empsmum, ecoactpa14 = w1empsdad)
  }
  if(wave == 2) {
    df <- df %>% rename(ecoactma15 = w2empsmum, ecoactpa15 = w2empsdad)
  }
  if(wave == 3) {
    df <- df %>% rename(ecoactma16 = w3empsmum, ecoactpa16 = w3empsdad)
  }
  if(wave == 4) {
    df <- df %>% rename(ecoactma17 = w4empsmum, ecoactpa17 = w4empsdad)
  }
  return(df)
}

w1 <- load_wave("data/input/wave_one_lsype_family_background_2020.tab", 1)
w2 <- load_wave("data/input/wave_two_lsype_family_background_2020.tab", 2)
w3 <- load_wave("data/input/wave_three_lsype_family_background_2020.tab", 3)
w4 <- load_wave("data/input/wave_four_lsype_family_background_2020.tab", 4)

# Harmonise missing codes
w1 <- w1 %>% mutate(across(c(ecoactma14, ecoactpa14), map_missing))
w2 <- w2 %>% mutate(across(c(ecoactma15, ecoactpa15), map_missing))
w3 <- w3 %>% mutate(across(c(ecoactma16, ecoactpa16), map_missing))
w4 <- w4 %>% mutate(across(c(ecoactma17, ecoactpa17), map_missing))

# Merge waves by lower-case nsid
merged <- w1 %>% full_join(w2, by = "nsid") %>% full_join(w3, by = "nsid") %>% full_join(w4, by = "nsid")

# Select required variables
final_data <- merged %>% select(nsid, ecoactma14, ecoactpa14, ecoactma15, ecoactpa15, ecoactma16, ecoactpa16, ecoactma17, ecoactpa17)

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")