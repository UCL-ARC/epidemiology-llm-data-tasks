library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load datasets
wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave8_main <- read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Merge datasets using full_join by NSID
merged_data <- wave1 %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave8_main, by = "NSID") %>%
  full_join(wave8_derived, by = "NSID") %>%
  full_join(wave9_main, by = "NSID") %>%
  full_join(wave9_derived, by = "NSID")

# Function to map missing values to standard codes
map_missing <- function(var) {
  case_when(
    var %in% c(-9, -92) ~ -9,  # Refusal
    var %in% c(-8, -94) ~ -8,  # Don't know / insufficient information
    var %in% c(-7) ~ -7,       # Prefer not to say
    var %in% c(-3, -99) ~ -3,  # Not asked at fieldwork stage / not interviewed
    var %in% c(-2, -999, -998, -997, -995) ~ -2,  # Schedule not applicable / script error / information lost
    var %in% c(-1, -91) ~ -1,  # Item not applicable
    TRUE ~ var
  )
}

# Function to derive NVQ level from academic qualifications at age 25
derive_educ25 <- function(data) {
  educ25 <- rep(-3, nrow(data))
  for (i in 1:nrow(data)) {
    if (!is.na(data$W8DHANVQH[i])) {
      educ25[i] <- map_missing(data$W8DHANVQH[i])
      educ25[i] <- case_when(
        educ25[i] == 1 ~ 1,  # NVQ Level 1
        educ25[i] == 2 ~ 2,  # NVQ Level 2
        educ25[i] == 3 ~ 3,  # NVQ Level 3
        educ25[i] == 4 ~ 4,  # NVQ Level 4
        educ25[i] == 5 ~ 5,  # NVQ Level 5
        educ25[i] == 95 ~ 4, # Other academic qualification (map to NVQ Level 4)
        educ25[i] == 96 ~ 0, # None of these qualifications (map to NVQ Entry Level)
        TRUE ~ educ25[i]
      )
    }
  }
  educ25
}

# Function to derive NVQ level from academic qualifications at age 32
derive_educ32 <- function(data) {
  educ32 <- rep(-3, nrow(data))
  for (i in 1:nrow(data)) {
    if (!is.na(data$W9DANVQH[i])) {
      educ32[i] <- map_missing(data$W9DANVQH[i])
      educ32[i] <- case_when(
        educ32[i] == 1 ~ 1,  # NVQ Level 1
        educ32[i] == 2 ~ 2,  # NVQ Level 2
        educ32[i] == 3 ~ 3,  # NVQ Level 3
        educ32[i] == 4 ~ 4,  # NVQ Level 4
        educ32[i] == 5 ~ 5,  # NVQ Level 5
        educ32[i] == 95 ~ 4, # Other academic qualification (map to NVQ Level 4)
        educ32[i] == 96 ~ 0, # None of these qualifications (map to NVQ Entry Level)
        TRUE ~ educ32[i]
      )
    }
  }
  educ32
}

# Function to derive detailed academic qualifications at age 32
derive_educadtl32 <- function(data) {
  educadtl32 <- rep(-3, nrow(data))
  for (i in 1:nrow(data)) {
    if (!is.na(data$W9ACQU0A[i]) && data$W9ACQU0A[i] == 1) {
      educadtl32[i] <- 5  # Doctorate or equivalent
    } else if (!is.na(data$W9ACQU0B[i]) && data$W9ACQU0B[i] == 1) {
      educadtl32[i] <- 4  # Masters or equivalent
    } else if (!is.na(data$W9ACQU0C[i]) && data$W9ACQU0C[i] == 1) {
      educadtl32[i] <- 4  # Undergraduate or equivalent
    } else if (!is.na(data$W9ACQU0D[i]) && data$W9ACQU0D[i] == 1) {
      educadtl32[i] <- 4  # Post-graduate Diplomas and Certificates
    } else if (!is.na(data$W9ACQU0E[i]) && data$W9ACQU0E[i] == 1) {
      educadtl32[i] <- 4  # Diplomas in higher education and other higher education qualifications
    } else if (!is.na(data$W9ACQU0F[i]) && data$W9ACQU0F[i] == 1) {
      educadtl32[i] <- 3  # Teaching qualifications for schools or further education (below degree level)
    } else if (!is.na(data$W9ACQU0G[i]) && data$W9ACQU0G[i] == 1) {
      educadtl32[i] <- 3  # A/AS Levels or equivalent
    } else if (!is.na(data$W9ACQU0H[i]) && data$W9ACQU0H[i] == 1) {
      educadtl32[i] <- 2  # Grade A-C, Level 4-9
    } else if (!is.na(data$W9ACQU0I[i]) && data$W9ACQU0I[i] == 1) {
      educadtl32[i] <- 1  # Grade D-G, Level 1-3
    } else if (!is.na(data$W9ACQU0J[i]) && data$W9ACQU0J[i] == 1) {
      educadtl32[i] <- 3  # SCE Higher
    } else if (!is.na(data$W9ACQU0K[i]) && data$W9ACQU0K[i] == 1) {
      educadtl32[i] <- 4  # Scottish Certificate Sixth Year Studies
    } else if (!is.na(data$W9ACQU0L[i]) && data$W9ACQU0L[i] == 1) {
      educadtl32[i] <- 2  # SCE Standard
    } else if (!is.na(data$W9ACQU0M[i]) && data$W9ACQU0M[i] == 1) {
      educadtl32[i] <- 2  # National 4 and 5
    } else if (!is.na(data$W9ACQU0N[i]) && data$W9ACQU0N[i] == 1) {
      educadtl32[i] <- 1  # National 2 and 3
    } else if (!is.na(data$W9ACQU0O[i]) && data$W9ACQU0O[i] == 1) {
      educadtl32[i] <- 2  # Leaving Certificate
    } else if (!is.na(data$W9ACQU0P[i]) && data$W9ACQU0P[i] == 1) {
      educadtl32[i] <- 2  # Junior Certificate grade A-C
    } else if (!is.na(data$W9ACQU0Q[i]) && data$W9ACQU0Q[i] == 1) {
      educadtl32[i] <- 1  # Junior Certificate grade D and below
    } else if (!is.na(data$W9ACQU0R[i]) && data$W9ACQU0R[i] == 1) {
      educadtl32[i] <- 4  # Other academic qualifications (including overseas)
    } else if (!is.na(data$W9ACQU0S[i]) && data$W9ACQU0S[i] == 1) {
      educadtl32[i] <- 0  # None of these qualifications
    } else if (!is.na(data$W9ACQU0T[i]) && data$W9ACQU0T[i] == 1) {
      educadtl32[i] <- -8  # Don't know
    } else if (!is.na(data$W9ACQU0U[i]) && data$W9ACQU0U[i] == 1) {
      educadtl32[i] <- -9  # Refused
    } else if (!is.na(data$W9ACQU0V[i]) && data$W9ACQU0V[i] == 1) {
      educadtl32[i] <- -3  # No answer
    }
  }
  educadtl32
}

# Function to derive detailed vocational qualifications at age 32
derive_educvdtl32 <- function(data) {
  educvdtl32 <- rep(-3, nrow(data))
  for (i in 1:nrow(data)) {
    if (!is.na(data$W9VCQU0A[i]) && data$W9VCQU0A[i] == 1) {
      educvdtl32[i] <- 5  # Professional qualifications at degree level
    } else if (!is.na(data$W9VCQU0B[i]) && data$W9VCQU0B[i] == 1) {
      educvdtl32[i] <- 4  # Nursing or other medical qualifications (below degree level)
    } else if (!is.na(data$W9VCQU0C[i]) && data$W9VCQU0C[i] == 1) {
      educvdtl32[i] <- 4  # Level 4 or 5
    } else if (!is.na(data$W9VCQU0D[i]) && data$W9VCQU0D[i] == 1) {
      educvdtl32[i] <- 3  # Level 3
    } else if (!is.na(data$W9VCQU0E[i]) && data$W9VCQU0E[i] == 1) {
      educvdtl32[i] <- 2  # Level 2
    } else if (!is.na(data$W9VCQU0F[i]) && data$W9VCQU0F[i] == 1) {
      educvdtl32[i] <- 1  # Level 1
    } else if (!is.na(data$W9VCQU0G[i]) && data$W9VCQU0G[i] == 1) {
      educvdtl32[i] <- 3  # GNVQ Advanced
    } else if (!is.na(data$W9VCQU0H[i]) && data$W9VCQU0H[i] == 1) {
      educvdtl32[i] <- 2  # GNVQ Intermediate
    } else if (!is.na(data$W9VCQU0I[i]) && data$W9VCQU0I[i] == 1) {
      educvdtl32[i] <- 3  # Level 3
    } else if (!is.na(data$W9VCQU0J[i]) && data$W9VCQU0J[i] == 1) {
      educvdtl32[i] <- 2  # Level 2
    } else if (!is.na(data$W9VCQU0K[i]) && data$W9VCQU0K[i] == 1) {
      educvdtl32[i] <- 1  # Level Foundation
    } else if (!is.na(data$W9VCQU0L[i]) && data$W9VCQU0L[i] == 1) {
      educvdtl32[i] <- 3  # Advanced Craft, Part III
    } else if (!is.na(data$W9VCQU0M[i]) && data$W9VCQU0M[i] == 1) {
      educvdtl32[i] <- 2  # Craft, Part II
    } else if (!is.na(data$W9VCQU0N[i]) && data$W9VCQU0N[i] == 1) {
      educvdtl32[i] <- 1  # Craft, Part I
    } else if (!is.na(data$W9VCQU0O[i]) && data$W9VCQU0O[i] == 1) {
      educvdtl32[i] <- 3  # Level 3
    } else if (!is.na(data$W9VCQU0P[i]) && data$W9VCQU0P[i] == 1) {
      educvdtl32[i] <- 2  # Level 2
    } else if (!is.na(data$W9VCQU0Q[i]) && data$W9VCQU0Q[i] == 1) {
      educvdtl32[i] <- 1  # Level 1
    } else if (!is.na(data$W9VCQU0R[i]) && data$W9VCQU0R[i] == 1) {
      educvdtl32[i] <- 4  # Advanced Diploma
    } else if (!is.na(data$W9VCQU0S[i]) && data$W9VCQU0S[i] == 1) {
      educvdtl32[i] <- 4  # Higher Diploma
    } else if (!is.na(data$W9VCQU0T[i]) && data$W9VCQU0T[i] == 1) {
      educvdtl32[i] <- 3  # RSA Diploma
    } else if (!is.na(data$W9VCQU0U[i]) && data$W9VCQU0U[i] == 1) {
      educvdtl32[i] <- 2  # RSA Stage I, II, III
    } else if (!is.na(data$W9VCQU0V[i]) && data$W9VCQU0V[i] == 1) {
      educvdtl32[i] <- 4  # Higher Level BTEC
    } else if (!is.na(data$W9VCQU0W[i]) && data$W9VCQU0W[i] == 1) {
      educvdtl32[i] <- 3  # BTEC National
    } else if (!is.na(data$W9VCQU0X[i]) && data$W9VCQU0X[i] == 1) {
      educvdtl32[i] <- 2  # BTEC First
    } else if (!is.na(data$W9VCQU0Y[i]) && data$W9VCQU0Y[i] == 1) {
      educvdtl32[i] <- 3  # SCOTVEC National Certificate
    } else if (!is.na(data$W9VCQU0Z[i]) && data$W9VCQU0Z[i] == 1) {
      educvdtl32[i] <- 2  # SCOTVEC first or general diploma
    } else if (!is.na(data$W9VCQUAA[i]) && data$W9VCQUAA[i] == 1) {
      educvdtl32[i] <- 2  # SCOTVEC general diploma
    } else if (!is.na(data$W9VCQUAB[i]) && data$W9VCQUAB[i] == 1) {
      educvdtl32[i] <- 2  # SCOTVEC modules
    } else if (!is.na(data$W9VCQUAC[i]) && data$W9VCQUAC[i] == 1) {
      educvdtl32[i] <- 4  # HND or HNC
    } else if (!is.na(data$W9VCQUAD[i]) && data$W9VCQUAD[i] == 1) {
      educvdtl32[i] <- 3  # OND or ONCM
    } else if (!is.na(data$W9VCQUAE[i]) && data$W9VCQUAE[i] == 1) {
      educvdtl32[i] <- 1  # Junior certificate
    } else if (!is.na(data$W9VCQUAF[i]) && data$W9VCQUAF[i] == 1) {
      educvdtl32[i] <- 4  # Other vocational qualifications (including some overseas)
    } else if (!is.na(data$W9VCQUAG[i]) && data$W9VCQUAG[i] == 1) {
      educvdtl32[i] <- 0  # None of these qualifications
    } else if (!is.na(data$W9VCQUAH[i]) && data$W9VCQUAH[i] == 1) {
      educvdtl32[i] <- -8  # Don't know
    } else if (!is.na(data$W9VCQUAI[i]) && data$W9VCQUAI[i] == 1) {
      educvdtl32[i] <- -9  # Refused
    }
  }
  educvdtl32
}

# Derive variables
merged_data$educ25 <- derive_educ25(merged_data)
merged_data$educ32 <- derive_educ32(merged_data)
merged_data$educadtl32 <- derive_educadtl32(merged_data)
merged_data$educvdtl32 <- derive_educvdtl32(merged_data)

# Select only the required variables
cleaned_data <- merged_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")

# Return the path to the cleaned data
"data/output/cleaned_data.csv"
