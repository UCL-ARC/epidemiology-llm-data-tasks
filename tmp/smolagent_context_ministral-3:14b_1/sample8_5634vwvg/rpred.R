
library(haven)
library(dplyr)
library(readr)

# Load data files
wave8_main <- readr::read_delim("data/input/ns8_2015_main_interview.tab", delim = "\t")
wave8_derived <- readr::read_delim("data/input/ns8_2015_derived.tab", delim = "\t")
wave9_main <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9_derived <- readr::read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t")

# Convert NSID to character for consistent joining
wave8_main$NSID <- as.character(wave8_main$NSID)
wave8_derived$NSID <- as.character(wave8_derived$NSID)
wave9_main$NSID <- as.character(wave9_main$NSID)
wave9_derived$NSID <- as.character(wave9_derived$NSID)

# Merge datasets
merged_data <- full_join(wave8_main, wave8_derived, by = "NSID")
merged_data <- full_join(merged_data, wave9_main, by = "NSID")
merged_data <- full_join(merged_data, wave9_derived, by = "NSID")

# Function to create educ25 variable
create_educ25 <- function(data) {
  # Handle academic qualifications
  academic_nvq <- data$W8DHANVQH
  academic_nvq[academic_nvq == 95] <- 3  # Map other qualifications

  # Handle vocational qualifications
  vocational_vars <- c("W8VCQU0I", "W8VCQU0J", "W8VCQU0K", "W8VCQU0L", "W8VCQU0M",
                       "W8VCQU0N", "W8VCQU0O")
  vocational_nvq <- sapply(vocational_vars, function(var) {
    if(var %in% names(data)) {
      if(any(data[[var]] == 1, na.rm = TRUE)) {
        case_when(
          var == "W8VCQU0I" ~ 1,  # NVQ Level 1-2
          var %in% c("W8VCQU0J", "W8VCQU0K", "W8VCQU0L", "W8VCQU0M", "W8VCQU0N") ~ 4,  # NVQ Level 3-5
          var == "W8VCQU0O" ~ 3   # Other vocational
        )
      } else {
        NA
      }
    } else {
      NA
    }
  })
  max_vocational_nvq <- max(vocational_nvq, na.rm = TRUE)

  # Combine and collapse
  combined_nvq <- pmax(academic_nvq, max_vocational_nvq, na.rm = TRUE)
  collapsed_nvq <- case_when(
    combined_nvq %in% c(4, 5) ~ 0,  # NVQ 4-5
    combined_nvq %in% c(1, 2, 3) ~ 1,  # NVQ 1-3
    combined_nvq == 0 ~ 2,  # Entry level
    combined_nvq == 96 ~ 4,  # None
    TRUE ~ 3  # Other
  )

  # Handle missing values
  collapsed_nvq[is.na(collapsed_nvq)] <- -3

  # Create factor
  factor(collapsed_nvq,
         levels = c(0, 1, 2, 3, 4, -9, -8, -3, -2, -1),
         labels = c("NVQ 4-5", "NVQ 1-3", "Entry level", "Other", "None", "Refused", "-8", "-3", "-2", "-1"))
}

# Function to create educ32 variable
create_educ32 <- function(data) {
  combined_nvq <- pmax(data$W9DANVQH, data$W9DVNVQH, na.rm = TRUE)
  collapsed_nvq <- case_when(
    combined_nvq %in% c(4, 5) ~ 0,  # NVQ 4-5
    combined_nvq %in% c(1, 2, 3) ~ 1,  # NVQ 1-3
    combined_nvq == 0 ~ 2,  # Entry level
    combined_nvq == 95 ~ 3,  # Other
    combined_nvq == 96 ~ 4,  # None
    TRUE ~ 3  # Other
  )

  # Handle missing values
  collapsed_nvq[is.na(collapsed_nvq)] <- -3

  # Create factor
  factor(collapsed_nvq,
         levels = c(0, 1, 2, 3, 4, -9, -8, -3, -2, -1),
         labels = c("NVQ 4-5", "NVQ 1-3", "Entry level", "Other", "None", "Refused", "-8", "-3", "-2", "-1"))
}

# Function to create detailed academic qualifications
create_educadtl32 <- function(data) {
  ac_vars <- data %>% select(matches("^W9ACQU"))

  # Find first 'Yes' response
  for(var in names(ac_vars)) {
    if(any(data[[var]] == 1, na.rm = TRUE)) {
      labels <- c(
        "W9ACQU0A" = "Doctorate or equivalent",
        "W9ACQU0B" = "Masters or equivalent",
        "W9ACQU0C" = "Undergraduate or equivalent",
        "W9ACQU0D" = "Post-graduate Diplomas and Certificates",
        "W9ACQU0E" = "Diplomas in higher education",
        "W9ACQU0F" = "Teaching qualifications",
        "W9ACQU0G" = "A/AS Levels or equivalent",
        "W9ACQU0H" = "Grade A-C, Level 4-9",
        "W9ACQU0I" = "Grade D-G, Level 1-3",
        "W9ACQU0J" = "SCE Higher",
        "W9ACQU0K" = "Scottish Certificate Sixth Year Studies",
        "W9ACQU0L" = "SCE Standard",
        "W9ACQU0M" = "National 4 and 5",
        "W9ACQU0N" = "National 2 and 3",
        "W9ACQU0O" = "Leaving Certificate",
        "W9ACQU0P" = "Junior Certificate grade A-C",
        "W9ACQU0Q" = "Junior Certificate grade D and below",
        "W9ACQU0R" = "Other academic qualifications",
        "W9ACQU0S" = "None of these qualifications"
      )
      return(labels[var])
    }
  }

  # If no 'Yes' responses
  if(all(sapply(ac_vars, function(x) all(x %in% c(2, -3, -1, NA))))) {
    return("None of these qualifications")
  } else {
    return(NA_character_)
  }
}

# Function to create detailed vocational qualifications
create_educvdtl32 <- function(data) {
  vc_vars <- data %>% select(matches("^W9VCQU"))

  # Find first 'Yes' response
  for(var in names(vc_vars)) {
    if(any(data[[var]] == 1, na.rm = TRUE)) {
      labels <- c(
        "W9VCQU0A" = "Professional qualifications at degree level",
        "W9VCQU0B" = "Nursing or other medical qualifications",
        "W9VCQU0C" = "NVQ Level 4 or 5",
        "W9VCQU0D" = "NVQ Level 3",
        "W9VCQU0E" = "NVQ Level 2",
        "W9VCQU0F" = "NVQ Level 1",
        "W9VCQU0G" = "GNVQ Advanced",
        "W9VCQU0H" = "GNVQ Intermediate",
        "W9VCQU0I" = "Level Foundation",
        "W9VCQU0J" = "Advanced Craft, Part III",
        "W9VCQU0K" = "Craft, Part II",
        "W9VCQU0L" = "Craft, Part I",
        "W9VCQU0M" = "Advanced Diploma",
        "W9VCQU0N" = "Higher Diploma",
        "W9VCQU0O" = "RSA Diploma",
        "W9VCQU0P" = "RSA Stage I, II, III",
        "W9VCQU0Q" = "Higher Level BTEC",
        "W9VCQU0R" = "BTEC National",
        "W9VCQU0S" = "BTEC First",
        "W9VCQU0T" = "SCOTVEC National Certificate",
        "W9VCQU0U" = "SCOTVEC first or general diploma",
        "W9VCQU0V" = "SCOTVEC general diploma",
        "W9VCQU0W" = "SCOTVEC modules",
        "W9VCQU0X" = "HND or HNC",
        "W9VCQU0Y" = "OND or ONCM",
        "W9VCQU0Z" = "Junior certificate",
        "W9VCQUAA" = "Other vocational qualifications",
        "W9VCQUAB" = "None of these qualifications"
      )
      return(labels[var])
    }
  }

  # If no 'Yes' responses
  if(all(sapply(vc_vars, function(x) all(x %in% c(2, -3, -1, NA))))) {
    return("None of these qualifications")
  } else {
    return(NA_character_)
  }
}

# Apply functions to each row
final_data <- merged_data %>%
  mutate(
    educ25 = mapply(create_educ25, lapply(1:nrow(.), function(i) select(., NSID, W8DHANVQH, starts_with("W8VCQU"))[i, ])),
    educ32 = mapply(create_educ32, lapply(1:nrow(.), function(i) select(., NSID, W9DANVQH, W9DVNVQH)[i, ])),
    educadtl32 = mapply(create_educadtl32, lapply(1:nrow(.), function(i) select(., starts_with("W9ACQU"))[i, ])),
    educvdtl32 = mapply(create_educvdtl32, lapply(1:nrow(.), function(i) select(., starts_with("W9VCQU"))[i, ]))
  )

# Select only required variables
final_data <- final_data %>%
  select(NSID, educ25, educ32, educadtl32, educvdtl32)

# Write output
write_csv(final_data, "data/output/cleaned_data.csv")
message("Cleaned dataset has been written to data/output/cleaned_data.csv")
