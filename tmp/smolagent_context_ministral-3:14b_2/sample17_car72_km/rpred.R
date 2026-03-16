library(haven);library(dplyr);library(readr)

# Load datasets
wave_two <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
ns9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Function to handle missing values
recode_missing <- function(x) {
  x[x == -94] <- -8
  x[is.na(x)] <- -3
  return(x)
}

# Create empty data frame
final_data <- data.frame(NSID = character(0))

# Add imd15 from wave_two if exists
if("IMDRSCORE" %in% colnames(wave_two)) {
  temp <- wave_two %>% 
    mutate(imd15 = recode_missing(IMDRSCORE)) %>% 
    select(NSID, imd15)
  final_data <- full_join(final_data, temp, by = "NSID")
}

# Add imd16 from wave_three if exists
if("IMDRSCORE" %in% colnames(wave_three)) {
  temp <- wave_three %>% 
    mutate(imd16 = recode_missing(IMDRSCORE)) %>% 
    select(NSID, imd16)
  final_data <- full_join(final_data, temp, by = "NSID")
}

# Add imd32 from ns9 if exists
if("W9DIMDD" %in% colnames(ns9)) {
  temp <- ns9 %>% 
    mutate(imd32 = recode_missing(W9DIMDD)) %>% 
    select(NSID, imd32)
  final_data <- full_join(final_data, temp, by = "NSID")
}

# Write output
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)