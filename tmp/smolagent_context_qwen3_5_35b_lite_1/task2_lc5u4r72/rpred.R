library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all files from data/input/
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge all datasets by NSID
merged <- full_join(wave1, wave2, by = "NSID")
merged <- full_join(merged, wave4, by = "NSID")
merged <- full_join(merged, wave8, by = "NSID")
merged <- full_join(merged, wave9, by = "NSID")

# Function to convert missing values to standard codes based on label meanings
convert_missing <- function(x, label_map) {
  result <- x
  for (code in names(label_map)) {
    code_num <- as.numeric(code)
    label_meaning <- label_map[[code]]
    
    if (label_meaning %in% c("Refusal", "Refused")) {
      result <- ifelse(x == code_num, -9, result)
    } else if (label_meaning %in% c("Insufficient information", "Insufficient info")) {
      result <- ifelse(x == code_num, -8, result)
    } else if (label_meaning == "Prefer not to say" || label_meaning == "Don't want to answer") {
      result <- ifelse(x == code_num, -7, result)
    } else if (label_meaning %in% c("Not asked at the fieldwork stage", "Not interviewed", "Interviewer missed question")) {
      result <- ifelse(x == code_num, -3, result)
    } else if (label_meaning %in% c("Schedule not applicable", "Script error", "Missing history section data - unexplained", "Missing - household data lost")) {
      result <- ifelse(x == code_num, -2, result)
    } else if (label_meaning == "Not applicable") {
      result <- ifelse(x == code_num, -1, result)
    } else if (label_meaning == "Don't know") {
      result <- ifelse(x == code_num, -8, result)
    }
  }
  return(result)
}

# Wave 1 ethnicity (Age 14): W1ethnic2YP
w1_eth <- merged$W1ethnic2YP
w1_label_map <- c(
  "-999" = "Missing - household data lost",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-1" = "Don't know"
)
w1_eth <- convert_missing(w1_eth, w1_label_map)

# Wave 2 ethnicity (Age 15): W2ethnicYP
w2_eth <- merged$W2ethnicYP
w2_label_map <- c(
  "-998" = "Interviewer missed question",
  "-997" = "Script error",
  "-995" = "Missing history section data - unexplained",
  "-99" = "YP not interviewed",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-1" = "Don't Know"
)
w2_eth <- convert_missing(w2_eth, w2_label_map)

# Wave 4 ethnicity (Age 17): w4ethnic2YP
w4_eth <- merged$w4ethnic2YP
w4_label_map <- c(
  "-94" = "Insufficient information",
  "-1" = "Don't know"
)
w4_eth <- convert_missing(w4_eth, w4_label_map)

# Wave 8 ethnicity (Age 25): W8DETHN15
w8_eth <- merged$W8DETHN15
w8_label_map <- c(
  "-9" = "Refused",
  "-8" = "Insufficient information",
  "-1" = "Not applicable"
)
w8_eth <- convert_missing(w8_eth, w8_label_map)

# Wave 9 ethnicity (Age 32): W9DETHN15
w9_eth <- merged$W9DETHN15
w9_label_map <- c(
  "-8" = "Insufficient information"
)
w9_eth <- convert_missing(w9_eth, w9_label_map)

# Consolidate ethnicity using earliest-valid-first rule
consolidate_earliest <- function(w1, w2, w4, w8, w9) {
  result <- rep(NA, length(w1))
  
  for (i in seq_along(w1)) {
    # Check each wave in order, use first valid substantive response
    if (!is.na(w1[i]) && w1[i] > 0) {
      result[i] <- w1[i]
    } else if (!is.na(w2[i]) && w2[i] > 0) {
      result[i] <- w2[i]
    } else if (!is.na(w4[i]) && w4[i] > 0) {
      result[i] <- w4[i]
    } else if (!is.na(w8[i]) && w8[i] > 0) {
      result[i] <- w8[i]
    } else if (!is.na(w9[i]) && w9[i] > 0) {
      result[i] <- w9[i]
    } else {
      # No valid response, use the most recent missing code if available
      if (!is.na(w9[i]) && w9[i] < 0) {
        result[i] <- w9[i]
      } else if (!is.na(w8[i]) && w8[i] < 0) {
        result[i] <- w8[i]
      } else if (!is.na(w4[i]) && w4[i] < 0) {
        result[i] <- w4[i]
      } else if (!is.na(w2[i]) && w2[i] < 0) {
        result[i] <- w2[i]
      } else if (!is.na(w1[i]) && w1[i] < 0) {
        result[i] <- w1[i]
      }
    }
  }
  return(result)
}

# Create consolidated ethnicity variable
eth <- consolidate_earliest(w1_eth, w2_eth, w4_eth, w8_eth, w9_eth)

# Define value labels for the consolidated variable (following Wave 1 categories)
value_labels <- c(
  "1" = "White - British",
  "2" = "White - Irish",
  "3" = "Any other White background",
  "4" = "Mixed - White and Black Caribbean",
  "5" = "Mixed - White and Black African",
  "6" = "Mixed - White and Asian",
  "7" = "Any other mixed background",
  "8" = "Indian",
  "9" = "Pakistani",
  "10" = "Bangladeshi",
  "11" = "Any other Asian background",
  "12" = "Black Caribbean",
  "13" = "Black African",
  "14" = "Any other Black background",
  "15" = "Chinese",
  "16" = "Any other ethnic background"
)

# Convert NA to -3 for variables with no valid response
eth_final <- ifelse(is.na(eth), -3, eth)

# Create final dataframe with NSID and eth
cleaned_data <- data.frame(
  NSID = as.character(merged$NSID),
  eth = eth_final
)

# Apply labels using labelled package functions
attr(cleaned_data$eth, "label") <- "Ethnicity (consolidated across all waves)"
attr(cleaned_data$eth, "values") <- value_labels

# Write to output
write_csv(cleaned_data, "data/output/cleaned_data.csv")

cat("Successfully created cleaned_data.csv with", nrow(cleaned_data), "records\n")
