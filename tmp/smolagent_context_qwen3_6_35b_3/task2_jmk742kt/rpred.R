library(dplyr)
library(readr)
library(labelled)

# Load all files
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Merge all datasets by NSID
data <- full_join(w1, w2, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

# Map missing values for each source variable to standard codes
# W1ethnic2YP: -999=Missing(-2), -94=Insufficient(-8), -92=Refused(-9), -91=Not applicable(-1), -1=Don't know(-8)
data$W1ethnic2YP_clean <- data$W1ethnic2YP
data$W1ethnic2YP_clean[data$W1ethnic2YP == -999] <- -2
data$W1ethnic2YP_clean[data$W1ethnic2YP == -94] <- -8
data$W1ethnic2YP_clean[data$W1ethnic2YP == -92] <- -9
data$W1ethnic2YP_clean[data$W1ethnic2YP == -91] <- -1
data$W1ethnic2YP_clean[data$W1ethnic2YP == -1] <- -8

# W2ethnicYP: -998=Interviewer missed(-2), -997=Script error(-2), -995=Missing history(-2), -99=Not interviewed(-3), -92=Refused(-9), -91=Not applicable(-1), -1=Don't know(-8)
data$W2ethnicYP_clean <- data$W2ethnicYP
data$W2ethnicYP_clean[data$W2ethnicYP == -998] <- -2
data$W2ethnicYP_clean[data$W2ethnicYP == -997] <- -2
data$W2ethnicYP_clean[data$W2ethnicYP == -995] <- -2
data$W2ethnicYP_clean[data$W2ethnicYP == -99] <- -3
data$W2ethnicYP_clean[data$W2ethnicYP == -92] <- -9
data$W2ethnicYP_clean[data$W2ethnicYP == -91] <- -1
data$W2ethnicYP_clean[data$W2ethnicYP == -1] <- -8

# w4ethnic2YP: -94=Insufficient(-8), -1=Don't know(-8)
data$w4ethnic2YP_clean <- data$w4ethnic2YP
data$w4ethnic2YP_clean[data$w4ethnic2YP == -94] <- -8
data$w4ethnic2YP_clean[data$w4ethnic2YP == -1] <- -8

# W8DETHN15: -9=Refused(-9), -8=Insufficient(-8), -1=Not applicable(-1)
data$W8DETHN15_clean <- data$W8DETHN15
data$W8DETHN15_clean[data$W8DETHN15 == -9] <- -9
data$W8DETHN15_clean[data$W8DETHN15 == -8] <- -8
data$W8DETHN15_clean[data$W8DETHN15 == -1] <- -1

# W9DETHN15: -8=Insufficient(-8)
data$W9DETHN15_clean <- data$W9DETHN15
data$W9DETHN15_clean[data$W9DETHN15 == -8] <- -8

# Derive consolidated eth variable using earliest valid first
# Valid responses are 1-16
# Order: W1ethnic2YP -> W2ethnicYP -> w4ethnic2YP -> W8DETHN15 -> W9DETHN15

eth_values <- rep(NA_real_, nrow(data))

# First try W1ethnic2YP (valid: 1-16)
idx_w1 <- !is.na(data$W1ethnic2YP_clean) & data$W1ethnic2YP_clean >= 1 & data$W1ethnic2YP_clean <= 16
eth_values[idx_w1] <- data$W1ethnic2YP_clean[idx_w1]

# If W1 is missing, try W2ethnicYP
idx_w2 <- is.na(eth_values) & !is.na(data$W2ethnicYP_clean) & data$W2ethnicYP_clean >= 1 & data$W2ethnicYP_clean <= 16
eth_values[idx_w2] <- data$W2ethnicYP_clean[idx_w2]

# If W2 is missing, try w4ethnic2YP
idx_w4 <- is.na(eth_values) & !is.na(data$w4ethnic2YP_clean) & data$w4ethnic2YP_clean >= 1 & data$w4ethnic2YP_clean <= 16
eth_values[idx_w4] <- data$w4ethnic2YP_clean[idx_w4]

# If w4 is missing, try W8DETHN15
idx_w8 <- is.na(eth_values) & !is.na(data$W8DETHN15_clean) & data$W8DETHN15_clean >= 1 & data$W8DETHN15_clean <= 16
eth_values[idx_w8] <- data$W8DETHN15_clean[idx_w8]

# If W8 is missing, try W9DETHN15
idx_w9 <- is.na(eth_values) & !is.na(data$W9DETHN15_clean) & data$W9DETHN15_clean >= 1 & data$W9DETHN15_clean <= 16
eth_values[idx_w9] <- data$W9DETHN15_clean[idx_w9]

# For remaining NAs, determine the most appropriate missing code
for (i in seq_len(nrow(data))) {
  if (is.na(eth_values[i])) {
    if (!is.na(data$W1ethnic2YP_clean[i]) && data$W1ethnic2YP_clean[i] != -3) {
      eth_values[i] <- data$W1ethnic2YP_clean[i]
    } else if (!is.na(data$W2ethnicYP_clean[i]) && data$W2ethnicYP_clean[i] != -3) {
      eth_values[i] <- data$W2ethnicYP_clean[i]
    } else if (!is.na(data$w4ethnic2YP_clean[i]) && data$w4ethnic2YP_clean[i] != -3) {
      eth_values[i] <- data$w4ethnic2YP_clean[i]
    } else if (!is.na(data$W8DETHN15_clean[i]) && data$W8DETHN15_clean[i] != -3) {
      eth_values[i] <- data$W8DETHN15_clean[i]
    } else if (!is.na(data$W9DETHN15_clean[i]) && data$W9DETHN15_clean[i] != -3) {
      eth_values[i] <- data$W9DETHN15_clean[i]
    }
  }
}

# Create labelled variable with proper labels
# Create a named integer vector for labels
eth_labels <- setNames(
  c("White - British", "White - Irish", "Any other White background",
    "Mixed - White and Black Caribbean", "Mixed - White and Black African",
    "Mixed - White and Asian", "Any other mixed background", "Indian",
    "Pakistani", "Bangladeshi", "Any other Asian background", "Black Caribbean",
    "Black African", "Any other Black background", "Chinese", "Any other ethnic background"),
  as.character(1:16)
)

# Create labelled variable
data$eth <- haven::labelled(eth_values)

# Set labels using labelled package
attr(data$eth, "labels") <- eth_labels
attr(data$eth, "class") <- c("labelled", "numeric")

# Add value labels for missing values
val_labels <- c(`"-9"` = "Refusal", `"-8"` = "Don't know / insufficient information",
                `"-7"` = "Prefer not to say", `"-3"` = "Not asked at the fieldwork stage / not interviewed",
                `"-2"` = "Schedule not applicable / script error / information lost",
                `"-1"` = "Item not applicable")
attr(data$eth, "value.labels") <- val_labels

# Keep only NSID and eth
data_final <- data %>% select(NSID, eth)

# Write output
dir.exists("data/output")
write_csv(data_final, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(data_final), "\n")
cat("Number of valid responses:", sum(!is.na(data_final$eth) & !is.na(as.numeric(as.character(data_final$eth))) & as.numeric(as.character(data_final$eth)) >= 1 & as.numeric(as.character(data_final$eth)) <= 16), "\n")