library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))

detailed_mum <- list(
  detail_wave1 = wave1$W1hiqualmum,
  detail_wave2 = wave2$W2hiqualmum,
  detail_wave4 = wave4$w4hiqualmum
)

detailed_dad <- list(
  detail_wave1 = wave1$W1hiqualdad,
  detail_wave2 = wave2$W2hiqualdad,
  detail_wave4 = wave4$w4hiqualdad
)

harmonize_missing_mum <- function(x) {
  result <- as.character(x)
  result[result %in% c("-999", "-99", "-98", "-94", "-92", "-91", "-1")] <- result
  # Map specific values
  result[as.numeric(result) == -999.0] <- "-3"
  result[as.numeric(result) == -99.0] <- "-1"
  result[as.numeric(result) == -98.0] <- "-1"
  result[as.numeric(result) == -94.0] <- "-8"
  result[as.numeric(result) == -92.0] <- "-9"
  result[as.numeric(result) == -91.0] <- "-2"
  result[as.numeric(result) == -1.0] <- "-8"
  return(result)
}

harmonize_missing_dad <- function(x) {
  result <- as.character(x)
  result[as.numeric(result) == -999.0] <- "-3"
  result[as.numeric(result) == -99.0] <- "-1"
  result[as.numeric(result) == -98.0] <- "-1"
  result[as.numeric(result) == -94.0] <- "-8"
  result[as.numeric(result) == -92.0] <- "-9"
  result[as.numeric(result) == -91.0] <- "-2"
  result[as.numeric(result) == -1.0] <- "-8"
  return(result)
}

collapse_education <- function(x) {
  result <- as.character(x)
  result[as.numeric(result) %in% c(1.0, 2.0, 3.0, 4.0)] <- "0"
  result[as.numeric(result) %in% c(5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0)] <- "1"
  result[as.numeric(result) == 18.0] <- "2"
  result[as.numeric(result) == 19.0] <- "3"
  result[as.numeric(result) == 20.0] <- "4"
  return(result)
}

cons_mum <- detailed_mum$detail_wave1
n <- length(cons_mum)
for (w in names(detailed_mum)) {
  current_wave <- detailed_mum[[w]]
  for (i in 1:n) {
    if (is.na(cons_mum[i]) && !is.na(current_wave[i])) {
      cons_mum[i] <- current_wave[i]
    }
  }
}

cons_dad <- detailed_dad$detail_wave1
n <- length(cons_dad)
for (w in names(detailed_dad)) {
  current_wave <- detailed_dad[[w]]
  for (i in 1:n) {
    if (is.na(cons_dad[i]) && !is.na(current_wave[i])) {
      cons_dad[i] <- current_wave[i]
    }
  }
}

cons_mum_char <- harmonize_missing_mum(cons_mum)
cons_dad_char <- harmonize_missing_dad(cons_dad)

collapsed_mum <- collapse_education(cons_mum_char)
collapsed_dad <- collapse_education(cons_dad_char)

# Using labelled package functions directly
# Create character vectors with labels
educma_char <- collapsed_mum
educma <- labelled(educma_char, c(
  "0" = "NVQ 4-5: Degree level qualifications",
  "1" = "NVQ 1-3: Sub-degree qualifications",
  "2" = "None/entry: Training below NVQ level",
  "3" = "Other: Qualification level unspecified",
  "4" = "No qualifications mentioned",
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-2" = "Not applicable/schedule error/information lost",
  "-1" = "Item not applicable"
))

educpa_char <- collapsed_dad
educpa <- labelled(educpa_char, c(
  "0" = "NVQ 4-5: Degree level qualifications",
  "1" = "NVQ 1-3: Sub-degree qualifications",
  "2" = "None/entry: Training below NVQ level",
  "3" = "Other: Qualification level unspecified",
  "4" = "No qualifications mentioned",
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-2" = "Not applicable/schedule error/information lost",
  "-1" = "Item not applicable"
))

detail_mum_char <- cons_mum_char
detail_mum <- labelled(detail_mum_char, c(
  "1" = "Higher Degree",
  "2" = "First Degree",
  "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree",
  "6" = "Nursing qualification, non-degree",
  "7" = "A Levels",
  "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS",
  "11" = "Scottish Higher Grade",
  "12" = "AS Level",
  "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned",
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-2" = "Not applicable/schedule error/information lost",
  "-1" = "Item not applicable"
))

detail_dad_char <- cons_dad_char
detail_dad <- labelled(detail_dad_char, c(
  "1" = "Higher Degree",
  "2" = "First Degree",
  "3" = "HE Diploma",
  "4" = "HNC/HND/NVQ4",
  "5" = "Teaching qualification, non-degree",
  "6" = "Nursing qualification, non-degree",
  "7" = "A Levels",
  "8" = "OND/ONC",
  "9" = "City and guilds part III, NVQ3",
  "10" = "CSYS",
  "11" = "Scottish Higher Grade",
  "12" = "AS Level",
  "13" = "Trade apprenticeship",
  "14" = "City and guilds part II, NVQ2",
  "15" = "GCSE grade A-C and equivalent",
  "16" = "GCSE grade D-E and equivalent",
  "17" = "City and guilds part I, NVQ1",
  "18" = "Youth training, skill seekers",
  "19" = "Qualification, level unspecified",
  "20" = "No qualification mentioned",
  "-3" = "Not asked at fieldwork stage/participated/interviewed",
  "-9" = "Refusal",
  "-8" = "Don't know/insufficient information",
  "-2" = "Not applicable/schedule error/information lost",
  "-1" = "Item not applicable"
))

# Convert to factors
educma_factor <- as.factor(educma)
educpa_factor <- as.factor(educpa)
detail_mum_factor <- as.factor(detail_mum)
detail_dad_factor <- as.factor(detail_dad)

final_data <- data.frame(
  NSID = wave1$NSID,
  educma = educma_factor,
  educpa = educpa_factor,
  educdtlma = detail_mum_factor,
  educdtlpa = detail_dad_factor
)

write_csv(final_data, "data/output/cleaned_data.csv")

cat("Data cleaning complete!\n")
cat("Rows:", nrow(final_data), "\n")
cat("Columns:", names(final_data), "\n")