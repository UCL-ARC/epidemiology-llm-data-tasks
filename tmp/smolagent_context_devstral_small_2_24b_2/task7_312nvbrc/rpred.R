library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
wave8 <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t", show_col_types = FALSE)
wave9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge datasets using NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Define harmonised mapping function for educaim variables
harmonise_educaim <- function(data, wave, source_var, value_labels) {
  # Initialize output variable
  educaim_var <- rep(-3, nrow(data))
  
  # Map source variable to harmonised categories based on value labels
  for (i in 1:nrow(data)) {
    source_val <- data[[source_var]][i]
    
    # Skip missing values initially
    if (is.na(source_val) || source_val < 0) {
      next
    }
    
    # Map substantive codes based on value labels
    label <- value_labels[as.character(source_val)]
    
    if (wave == 17) {
      # Age 17: No NVQ 4-5 options
      if (label %in% c("NVQ 3", "AVCE", "A/AS", "Other level 3")) {
        educaim_var[i] <- 1
      } else if (label %in% c("NVQ 2", "Intermediate GNVQ", "Other level 2", "GCSE")) {
        educaim_var[i] <- 1
      } else if (label %in% c("NVQ 1", "Foundation", "Other level 1")) {
        educaim_var[i] <- 2
      } else if (label %in% c("Other", "No detail")) {
        educaim_var[i] <- 3
      } else if (!is.na(label) && label == "Not studying") {
        educaim_var[i] <- 5
      }
    } else if (wave == 19) {
      if (label %in% c("NVQ 5", "First/Other Degree", "NVQ 4", "Other HE")) {
        educaim_var[i] <- 0
      } else if (label %in% c("NVQ 3", "AVCE", "A/AS", "Other level 3")) {
        educaim_var[i] <- 1
      } else if (label %in% c("NVQ 2", "Other level 2", "GCSE")) {
        educaim_var[i] <- 1
      } else if (label %in% c("NVQ 1", "Other level 1")) {
        educaim_var[i] <- 2
      } else if (label %in% c("Other (level unknown)", "No detail")) {
        educaim_var[i] <- 3
      } else if (!is.na(label) && label == "Not studying") {
        educaim_var[i] <- 5
      }
    } else if (wave == 20) {
      if (label %in% c("NVQ 4", "First/Other Degree", "Other HE", "NVQ 5")) {
        educaim_var[i] <- 0
      } else if (label %in% c("NVQ 3", "A/AS", "AVCE", "Other level 3")) {
        educaim_var[i] <- 1
      } else if (label %in% c("NVQ 2", "GCSE", "Other level 2")) {
        educaim_var[i] <- 1
      } else if (label %in% c("NVQ 1", "Other level 1")) {
        educaim_var[i] <- 2
      } else if (label %in% c("Other (level unknown)")) {
        educaim_var[i] <- 3
      } else if (!is.na(label) && label == "Not applicable (not studying)") {
        educaim_var[i] <- 5
      }
    }
  }
  
  # Handle missing values
  educaim_var[is.na(data[[source_var]])] <- -3
  educaim_var[data[[source_var]] == -94] <- -8
  educaim_var[data[[source_var]] == -91] <- -1
  educaim_var[data[[source_var]] == -99] <- -3
  
  return(educaim_var)
}

# Derive educaim variables for each wave
# Wave 4 (Age 17)
wave4_value_labels <- c(
  "1" = "NVQ 3",
  "2" = "AVCE",
  "3" = "A/AS",
  "4" = "Other level 3",
  "5" = "NVQ 2",
  "6" = "Intermediate GNVQ",
  "7" = "Other level 2",
  "8" = "GCSE",
  "9" = "NVQ 1",
  "10" = "Foundation",
  "11" = "Other level 1",
  "12" = "Other",
  "13" = "No detail",
  "14" = "Not studying"
)
merged_data$educaim17 <- harmonise_educaim(merged_data, 17, "w4saim", wave4_value_labels)

# Wave 6 (Age 19)
wave6_value_labels <- c(
  "1" = "NVQ 5",
  "2" = "First/Other Degree",
  "3" = "NVQ 4",
  "4" = "Other HE",
  "5" = "NVQ 3",
  "6" = "AVCE",
  "7" = "A/AS",
  "8" = "Other level 3",
  "9" = "NVQ 2",
  "10" = "Other level 2",
  "11" = "GCSE",
  "12" = "NVQ 1",
  "13" = "Other level 1",
  "14" = "Other (level unknown)",
  "15" = "No detail",
  "16" = "Not studying"
)
merged_data$educaim19 <- harmonise_educaim(merged_data, 19, "W6Saim", wave6_value_labels)

# Wave 7 (Age 20)
wave7_value_labels <- c(
  "-94" = "Insufficient information",
  "-91" = "Not applicable (not studying)",
  "1" = "NVQ 1",
  "2" = "Other level 1",
  "3" = "NVQ 2",
  "4" = "GCSE",
  "5" = "Other level 2",
  "6" = "NVQ 3",
  "7" = "A/AS",
  "8" = "AVCE",
  "9" = "Other level 3",
  "10" = "NVQ 4",
  "11" = "First/Other Degree",
  "12" = "Other HE",
  "13" = "NVQ 5",
  "14" = "Other (level unknown)"
)
merged_data$educaim20 <- harmonise_educaim(merged_data, 20, "W7SAim", wave7_value_labels)

# Derive educaim variables for adult waves (25 and 32)
# Wave 8 (Age 25)
merged_data$educaim25 <- -3
for (i in 1:nrow(merged_data)) {
  # Check if not currently studying
  if (!is.na(merged_data$W8ACTIVITY05[i]) && merged_data$W8ACTIVITY05[i] == 0) {
    merged_data$educaim25[i] <- 5
  } else {
    # Check qualification indicators in priority order
    if (!is.na(merged_data$W8ACQUC0A[i]) && merged_data$W8ACQUC0A[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8ACQUC0B[i]) && merged_data$W8ACQUC0B[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8ACQUC0C[i]) && merged_data$W8ACQUC0C[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8ACQUC0D[i]) && merged_data$W8ACQUC0D[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8ACQUC0E[i]) && merged_data$W8ACQUC0E[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8VCQUC0J[i]) && merged_data$W8VCQUC0J[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8VCQUC0K[i]) && merged_data$W8VCQUC0K[i] == 1) {
      merged_data$educaim25[i] <- 0
    } else if (!is.na(merged_data$W8ACQUC0F[i]) && merged_data$W8ACQUC0F[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0G[i]) && merged_data$W8ACQUC0G[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0H[i]) && merged_data$W8ACQUC0H[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0I[i]) && merged_data$W8ACQUC0I[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0J[i]) && merged_data$W8ACQUC0J[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0K[i]) && merged_data$W8ACQUC0K[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0L[i]) && merged_data$W8ACQUC0L[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0M[i]) && merged_data$W8ACQUC0M[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0N[i]) && merged_data$W8ACQUC0N[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0A[i]) && merged_data$W8VCQUC0A[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0B[i]) && merged_data$W8VCQUC0B[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0C[i]) && merged_data$W8VCQUC0C[i] == 1) {
      merged_data$educaim25[i] <- 2
    } else if (!is.na(merged_data$W8VCQUC0D[i]) && merged_data$W8VCQUC0D[i] == 1) {
      merged_data$educaim25[i] <- 2
    } else if (!is.na(merged_data$W8VCQUC0E[i]) && merged_data$W8VCQUC0E[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0F[i]) && merged_data$W8VCQUC0F[i] == 1) {
      merged_data$educaim25[i] <- 2
    } else if (!is.na(merged_data$W8VCQUC0G[i]) && merged_data$W8VCQUC0G[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0H[i]) && merged_data$W8VCQUC0H[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0I[i]) && merged_data$W8VCQUC0I[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0K[i]) && merged_data$W8VCQUC0K[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8VCQUC0L[i]) && merged_data$W8VCQUC0L[i] == 1) {
      merged_data$educaim25[i] <- 1
    } else if (!is.na(merged_data$W8ACQUC0O[i]) && merged_data$W8ACQUC0O[i] == 1) {
      merged_data$educaim25[i] <- 4
    } else if (!is.na(merged_data$W8ACQUC0P[i]) && merged_data$W8ACQUC0P[i] == 1) {
      merged_data$educaim25[i] <- -8
    } else if (!is.na(merged_data$W8ACQUC0Q[i]) && merged_data$W8ACQUC0Q[i] == 1) {
      merged_data$educaim25[i] <- -9
    } else {
      merged_data$educaim25[i] <- 3
    }
  }
}

# Wave 9 (Age 32)
merged_data$educaim32 <- -3
for (i in 1:nrow(merged_data)) {
  # Check if not currently studying
  if (!is.na(merged_data$W9ECONACT2[i]) && !(merged_data$W9ECONACT2[i] %in% c(6, 7))) {
    merged_data$educaim32[i] <- 5
  } else {
    # Check qualification indicators in priority order
    if (!is.na(merged_data$W9ACQUC0A[i]) && merged_data$W9ACQUC0A[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9ACQUC0B[i]) && merged_data$W9ACQUC0B[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9ACQUC0C[i]) && merged_data$W9ACQUC0C[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9ACQUC0D[i]) && merged_data$W9ACQUC0D[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9ACQUC0E[i]) && merged_data$W9ACQUC0E[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9VCQUC0A[i]) && merged_data$W9VCQUC0A[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9VCQUC0B[i]) && merged_data$W9VCQUC0B[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9VCQUC0C[i]) && merged_data$W9VCQUC0C[i] == 1) {
      merged_data$educaim32[i] <- 0
    } else if (!is.na(merged_data$W9ACQUC0F[i]) && merged_data$W9ACQUC0F[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0G[i]) && merged_data$W9ACQUC0G[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0H[i]) && merged_data$W9ACQUC0H[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0I[i]) && merged_data$W9ACQUC0I[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0J[i]) && merged_data$W9ACQUC0J[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0K[i]) && merged_data$W9ACQUC0K[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0L[i]) && merged_data$W9ACQUC0L[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0M[i]) && merged_data$W9ACQUC0M[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0N[i]) && merged_data$W9ACQUC0N[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0O[i]) && merged_data$W9ACQUC0O[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0P[i]) && merged_data$W9ACQUC0P[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0Q[i]) && merged_data$W9ACQUC0Q[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9ACQUC0R[i]) && merged_data$W9ACQUC0R[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0D[i]) && merged_data$W9VCQUC0D[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0E[i]) && merged_data$W9VCQUC0E[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0F[i]) && merged_data$W9VCQUC0F[i] == 1) {
      merged_data$educaim32[i] <- 2
    } else if (!is.na(merged_data$W9VCQUC0G[i]) && merged_data$W9VCQUC0G[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0H[i]) && merged_data$W9VCQUC0H[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0I[i]) && merged_data$W9VCQUC0I[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0J[i]) && merged_data$W9VCQUC0J[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0K[i]) && merged_data$W9VCQUC0K[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0L[i]) && merged_data$W9VCQUC0L[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0M[i]) && merged_data$W9VCQUC0M[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0N[i]) && merged_data$W9VCQUC0N[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0O[i]) && merged_data$W9VCQUC0O[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0P[i]) && merged_data$W9VCQUC0P[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0Q[i]) && merged_data$W9VCQUC0Q[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0R[i]) && merged_data$W9VCQUC0R[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0S[i]) && merged_data$W9VCQUC0S[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0T[i]) && merged_data$W9VCQUC0T[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0U[i]) && merged_data$W9VCQUC0U[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0V[i]) && merged_data$W9VCQUC0V[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0W[i]) && merged_data$W9VCQUC0W[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0X[i]) && merged_data$W9VCQUC0X[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0Y[i]) && merged_data$W9VCQUC0Y[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUC0Z[i]) && merged_data$W9VCQUC0Z[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUCAA[i]) && merged_data$W9VCQUCAA[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUCAB[i]) && merged_data$W9VCQUCAB[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUCAC[i]) && merged_data$W9VCQUCAC[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUCAD[i]) && merged_data$W9VCQUCAD[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUCAE[i]) && merged_data$W9VCQUCAE[i] == 1) {
      merged_data$educaim32[i] <- 1
    } else if (!is.na(merged_data$W9VCQUCAF[i]) && merged_data$W9VCQUCAF[i] == 1) {
      merged_data$educaim32[i] <- 3
    } else if (!is.na(merged_data$W9VCQUCAG[i]) && merged_data$W9VCQUCAG[i] == 1) {
      merged_data$educaim32[i] <- 4
    } else if (!is.na(merged_data$W9VCQUCAH[i]) && merged_data$W9VCQUCAH[i] == 1) {
      merged_data$educaim32[i] <- -8
    } else if (!is.na(merged_data$W9VCQUCAI[i]) && merged_data$W9VCQUCAI[i] == 1) {
      merged_data$educaim32[i] <- -9
    } else {
      merged_data$educaim32[i] <- 3
    }
  }
}

# Select only NSID and derived educaim variables
output_data <- merged_data %>% select(NSID, educaim17, educaim19, educaim20, educaim25, educaim32)

# Write output
write_csv(output_data, "data/output/cleaned_data.csv")
