library(readr)
library(dplyr)

# Function to convert raw sweep codes to standard missing codes
map_missing <- function(x, wave){
  y <- x
  if(wave %in% c("14","15")){
    y[y == -3] <- -1        # Not yet paid -> Item not applicable
    y[y == -1] <- -8        # Don\'t know -> Don\'t know / insufficient information
    y[y == -992] <- -9       # No information – refused
  }
  if(wave == "16"){
    y[y == -3] <- -1
  }
  if(wave == "17"){
    y[y == -996] <- -3
  }
  y[y == -99] <- -3
  y[y == -92] <- -9
  y[y == -94] <- -8
  y[y == -91] <- -1
  y[y == -999] <- -2
  y[is.na(y)] <- -3
  return(y)
}

# Load all wave files
w1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols())
w2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols())
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols())
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols())

# Keep only needed columns
w1_sel <- w1 %>% select(NSID, W1GrsswkHH)
w2_sel <- w2 %>% select(NSID, W2GrsswkHH)
w3_sel <- w3 %>% select(NSID, W3incestw)
w4_sel <- w4 %>% select(NSID, w4IncEstW)

# Merge all waves by NSID
merged <- w1_sel %>% full_join(w2_sel, by = "NSID") %>%
  full_join(w3_sel, by = "NSID") %>%
  full_join(w4_sel, by = "NSID")

# Map missing and keep raw values
merged <- merged %>% mutate(
  incwhhcnt14_raw = W1GrsswkHH,
  incwhhcnt14 = map_missing(incwhhcnt14_raw, "14"),
  incwhhcnt15_raw = W2GrsswkHH,
  incwhhcnt15 = map_missing(incwhhcnt15_raw, "15"),
  incwhh16_raw = W3incestw,
  incwhh16 = map_missing(incwhh16_raw, "16"),
  incwhh17_raw = w4IncEstW,
  incwhh17 = map_missing(incwhh17_raw, "17")
)

# Function to band positive income values for 14 and 15
band_income <- function(x){
  case_when(
    is.na(x) ~ -3,
    x <= 49 ~ 1,
    x <= 99 ~ 2,
    x <= 199 ~ 3,
    x <= 299 ~ 4,
    x <= 399 ~ 5,
    x <= 499 ~ 6,
    x <= 599 ~ 7,
    x <= 699 ~ 8,
    x <= 799 ~ 9,
    x <= 899 ~ 10,
    x <= 999 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ -3
  )
}

merged <- merged %>% mutate(
  incwhh14 = band_income(incwhhcnt14),
  incwhh15 = band_income(incwhhcnt15)
)

# Levels and labels for banded variables
levels_vec <- c(1:12, -1, -8, -9, -3, -2)
labels_vec <- c(
  "Up to £49",
  "£50 up to £99",
  "£100 up to £199",
  "£200 up to £299",
  "£300 up to £399",
  "£400 up to £499",
  "£500 up to £599",
  "£600 up to £699",
  "£700 up to £799",
  "£800 up to £899",
  "£900 up to £999",
  "£1,000 or more",
  "Item not applicable",
  "Don\'t know / insufficient information",
  "Refusal",
  "Not asked at the fieldwork stage",
  "Schedule not applicable / script error / information lost"
)

# Convert to labelled factors
merged <- merged %>% mutate(
  incwhh14 = factor(incwhh14, levels = levels_vec, labels = labels_vec, ordered = FALSE),
  incwhh15 = factor(incwhh15, levels = levels_vec, labels = labels_vec, ordered = FALSE),
  incwhh16 = factor(incwhh16, levels = levels_vec, labels = labels_vec, ordered = FALSE),
  incwhh17 = factor(incwhh17, levels = levels_vec, labels = labels_vec, ordered = FALSE)
)

# Select final variables and write CSV
output <- merged %>% select(NSID, incwhh14, incwhh15, incwhh16, incwhh17, incwhhcnt14, incwhhcnt15)

write_csv(output, "data/output/cleaned_data.csv")
