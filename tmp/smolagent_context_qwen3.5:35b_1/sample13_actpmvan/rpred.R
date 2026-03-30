library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Load all five wave files
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Merge all datasets by NSID using full_join
data <- full_join(wave1, wave2, by = "NSID")
data <- full_join(data, wave3, by = "NSID")
data <- full_join(data, wave4, by = "NSID")
data <- full_join(data, wave5, by = "NSID")

# Function to recode NS-SEC values
recode_nssec <- function(x) {
  # Take integer part
  x_int <- as.integer(x)
  
  # Recode missing value codes
  # -999 → -2 (Script error/information lost)
  # -94 → -8 (Don't know/insufficient information)
  # -99, -98, and NA → -3 (Not asked/not interviewed)
  # Preserve: -9 (Refusal), -8 (Don't know), -7 (Prefer not to say), -3 (Not asked/not interviewed), -2 (Script error/information lost), -1 (Not applicable)
  
  x_recode <- x_int
  x_recode[x_int == -999] <- -2
  x_recode[x_int == -94] <- -8
  x_recode[x_int == -99] <- -3
  x_recode[x_int == -98] <- -3
  x_recode[is.na(x_int)] <- -3
  
  # Keep only valid categories 1-17, recode others to -3
  x_recode[x_recode > 17 | x_recode < -9 | (x_recode > 0 & x_recode < 1)] <- -3
  
  return(x_recode)
}

# Function to create labelled factor with NS-SEC labels
create_nssec_factor <- function(x) {
  # Create value labels as character keys
  value_labels <- c(
    "1" = "Employers in large organisations",
    "2" = "Higher managerial occupations",
    "3" = "Higher professional occupations",
    "4" = "Lower professional occupations",
    "5" = "Lower managerial occupations",
    "6" = "Higher supervisory occupations",
    "7" = "Intermediate occupations",
    "8" = "Employers in small organisations",
    "9" = "Own account workers",
    "10" = "Lower supervisory occupations",
    "11" = "Lower technical occupations",
    "12" = "Semi-routine occupations",
    "13" = "Routine occupations",
    "14" = "Never worked/Long-term unemployed/Not currently working",
    "15" = "Full-time students",
    "16" = "Not classified or inadequately stated",
    "17" = "Not classifiable for other reasons",
    "-9" = "Refusal",
    "-8" = "Don't know",
    "-7" = "Prefer not to say",
    "-3" = "Not asked/not interviewed",
    "-2" = "Script error/information lost",
    "-1" = "Not applicable"
  )
  
  # Convert to character first, then apply labels
  x <- as.character(x)
  x <- labelled(x, labels = value_labels)
  return(x)
}

# Apply recoding and renaming for each wave
# Wave 1 (Age 14)
data$nssecma14 <- recode_nssec(data$W1nsseccatmum)
data$nssecpa14 <- recode_nssec(data$W1nsseccatdad)

# Wave 2 (Age 15)
data$nssecma15 <- recode_nssec(data$W2nsseccatmum)
data$nssecpa15 <- recode_nssec(data$W2nsseccatdad)

# Wave 3 (Age 16)
data$nssecma16 <- recode_nssec(data$W3cnsseccatmum)
data$nssecpa16 <- recode_nssec(data$W3cnsseccatdad)

# Wave 4 (Age 17)
data$nssecma17 <- recode_nssec(data$w4cnsseccatmum)
data$nssecpa17 <- recode_nssec(data$w4cnsseccatdad)

# Wave 5 (Age 18)
data$nssecma18 <- recode_nssec(data$w5Cnsseccatmum)
data$nssecpa18 <- recode_nssec(data$w5Cnsseccatdad)

# Select only required variables
data_clean <- data %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# Convert all NS-SEC variables to labelled factors
for (var in c("nssecma14", "nssecpa14", "nssecma15", "nssecpa15", "nssecma16", "nssecpa16", "nssecma17", "nssecpa17", "nssecma18", "nssecpa18")) {
  data_clean[[var]] <- create_nssec_factor(data_clean[[var]])
}

# Create output directory if it doesn't exist
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# Write to CSV
write_csv(data_clean, "data/output/cleaned_data.csv")

# Print summary
cat("Data cleaning complete. Output written to data/output/cleaned_data.csv\n")
cat("Number of observations:", nrow(data_clean), "\n")
cat("Number of variables:", ncol(data_clean), "\n")
