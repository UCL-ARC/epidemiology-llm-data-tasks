library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Create output directory
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

input_dir <- "data/input"

# Load all waves
wave1_data <- read_delim(file = paste0(input_dir, "/wave_one_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)
wave2_data <- read_delim(file = paste0(input_dir, "/wave_two_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)
wave4_data <- read_delim(file = paste0(input_dir, "/wave_four_lsype_family_background_2020.tab"), delim = "\t", show_col_types = FALSE)

# Start with wave1 as base
final_nsid <- wave1_data$NSID

# Create wave1 dataframe
df_w1 <- data.frame(NSID = final_nsid, mum = wave1_data$W1hiqualmum, dad = wave1_data$W1hiqualdad)

# Create wave2 dataframe
df_w2 <- data.frame(NSID = wave2_data$NSID, mum = wave2_data$W2hiqualmum, dad = wave2_data$W2hiqualdad)

# Create wave4 dataframe
df_w4 <- data.frame(NSID = wave4_data$NSID, mum = wave4_data$w4hiqualmum, dad = wave4_data$w4hiqualdad)

# Full join all waves
df_combined <- full_join(df_w1, df_w2, by = "NSID", suffix = c("_w1", "_w2")) %>%
  full_join(df_w4, by = "NSID", suffix = c("_w1_w2", "_w4"))

# Consolidate maternal education: priority w1 > w2 > w4
df_combined$educdtlma <- ifelse(!is.na(df_combined$mum_w1) & df_combined$mum_w1 > 0, df_combined$mum_w1,
                                ifelse(!is.na(df_combined$mum_w2) & df_combined$mum_w2 > 0, df_combined$mum_w2,
                                       ifelse(!is.na(df_combined$mum_w4) & df_combined$mum_w4 > 0, df_combined$mum_w4, NA)))
df_combined$educdtlma[is.na(df_combined$educdtlma)] <- -3

# Consolidate paternal education
df_combined$educdtlpa <- ifelse(!is.na(df_combined$dad_w1) & df_combined$dad_w1 > 0, df_combined$dad_w1,
                                ifelse(!is.na(df_combined$dad_w2) & df_combined$dad_w2 > 0, df_combined$dad_w2,
                                       ifelse(!is.na(df_combined$dad_w4) & df_combined$dad_w4 > 0, df_combined$dad_w4, NA)))
df_combined$educdtlpa[is.na(df_combined$educdtlpa)] <- -3

# Map detailed to collapsed
map_to_collapsed <- function(x) {
  result <- integer(length(x))
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || val == -3) result[i] <- -3
    else if (val < 0) result[i] <- val
    else {
      if (val >= 1 && val <= 6) result[i] <- 0
      else if (val >= 7 && val <= 17) result[i] <- 1
      else if (val == 18) result[i] <- 2
      else if (val == 19) result[i] <- 3
      else if (val == 20) result[i] <- 4
      else result[i] <- NA_integer_
    }
  }
  return(result)
}

educdtlma <- df_combined$educdtlma
educdtlpa <- df_combined$educdtlpa
educma <- map_to_collapsed(educdtlma)
educpa <- map_to_collapsed(educdtlpa)

# Detailed labels (28 values: 7 negative + 1 positive + 20 positive + 1 for -3)
detailed_labels <- c(
  "-999" = "Missing - household data lost",
  "-99" = "Not interviewed",
  "-98" = "Not present",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-1" = "Don't know",
  "1.0" = "Higher Degree",
  "2.0" = "First Degree",
  "3.0" = "HE Diploma",
  "4.0" = "HNC/HND/NVQ4",
  "5.0" = "Teaching qualification, non-degree",
  "6.0" = "Nursing qualification, non-degree",
  "7.0" = "A Levels",
  "8.0" = "OND/ONC",
  "9.0" = "City and guilds part III, NVQ3",
  "10.0" = "CSYS",
  "11.0" = "Scottish Higher Grade",
  "12.0" = "AS Level",
  "13.0" = "Trade apprenticeship",
  "14.0" = "City and guilds part II, NVQ2",
  "15.0" = "GCSE grade A-C and equivalent",
  "16.0" = "GCSE grade D-E and equivalent",
  "17.0" = "City and guilds part I, NVQ1",
  "18.0" = "Youth training, skill seekers",
  "19.0" = "Qualification, level unspecified",
  "20.0" = "No qualification mentioned",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed"
)

# Collapsed labels (9 values: negative codes + 0-4 + -3)
collapsed_labels <- c(
  "-999" = "Missing - household data lost",
  "-99" = "Not interviewed",
  "-98" = "Not present",
  "-94" = "Insufficient information",
  "-92" = "Refused",
  "-91" = "Not applicable",
  "-1" = "Don't know",
  "0" = "NVQ 4-5: degree-level qualifications and above",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training programmes below NVQ level",
  "3" = "Other: qualifications where the level is unspecified",
  "4" = "No qualifications mentioned",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed"
)

# Create final dataset
final_data <- data.frame(
  NSID = df_combined$NSID,
  educdtlma = educdtlma,
  educdtlpa = educdtlpa,
  educma = educma,
  educpa = educpa
)

final_data <- final_data %>%
  mutate(
    educdtlma = factor(educdtlma, levels = c(-999, -99, -98, -94, -92, -91, -1, 1:20, -3),
                       labels = c("-999" = "Missing - household data lost", "-99" = "Not interviewed", "-98" = "Not present", "-94" = "Insufficient information", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4", "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree", "7" = "A Levels", "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3", "10" = "CSYS", "11" = "Scottish Higher Grade", "12" = "AS Level", "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2", "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent", "17" = "City and guilds part I, NVQ1", "18" = "Youth training, skill seekers", "19" = "Qualification, level unspecified", "20" = "No qualification mentioned", "-3" = "Not asked at the fieldwork stage/participated/interviewed")),
    educdtlpa = factor(educdtlpa, levels = c(-999, -99, -98, -94, -92, -91, -1, 1:20, -3),
                       labels = c("-999" = "Missing - household data lost", "-99" = "Not interviewed", "-98" = "Not present", "-94" = "Insufficient information", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "1" = "Higher Degree", "2" = "First Degree", "3" = "HE Diploma", "4" = "HNC/HND/NVQ4", "5" = "Teaching qualification, non-degree", "6" = "Nursing qualification, non-degree", "7" = "A Levels", "8" = "OND/ONC", "9" = "City and guilds part III, NVQ3", "10" = "CSYS", "11" = "Scottish Higher Grade", "12" = "AS Level", "13" = "Trade apprenticeship", "14" = "City and guilds part II, NVQ2", "15" = "GCSE grade A-C and equivalent", "16" = "GCSE grade D-E and equivalent", "17" = "City and guilds part I, NVQ1", "18" = "Youth training, skill seekers", "19" = "Qualification, level unspecified", "20" = "No qualification mentioned", "-3" = "Not asked at the fieldwork stage/participated/interviewed")),
    educma = factor(educma, levels = sort(unique(c(educma, -999, -99, -98, -94, -92, -91, -1, 0:4, -3))),
                    labels = c("-999" = "Missing - household data lost", "-99" = "Not interviewed", "-98" = "Not present", "-94" = "Insufficient information", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "0" = "NVQ 4-5: degree-level qualifications and above", "1" = "NVQ 1-3: sub-degree qualifications", "2" = "None/entry: training programmes below NVQ level", "3" = "Other: qualifications where the level is unspecified", "4" = "No qualifications mentioned", "-3" = "Not asked at the fieldwork stage/participated/interviewed")),
    educpa = factor(educpa, levels = sort(unique(c(educpa, -999, -99, -98, -94, -92, -91, -1, 0:4, -3))),
                    labels = c("-999" = "Missing - household data lost", "-99" = "Not interviewed", "-98" = "Not present", "-94" = "Insufficient information", "-92" = "Refused", "-91" = "Not applicable", "-1" = "Don't know", "0" = "NVQ 4-5: degree-level qualifications and above", "1" = "NVQ 1-3: sub-degree qualifications", "2" = "None/entry: training programmes below NVQ level", "3" = "Other: qualifications where the level is unspecified", "4" = "No qualifications mentioned", "-3" = "Not asked at the fieldwork stage/participated/interviewed"))
  )

write_csv(final_data, "data/output/cleaned_data.csv")

cat("Processing complete.\n")
cat("Variables:", names(final_data), "\n")
cat("N rows:", nrow(final_data), "\n")
cat("Output written to: data/output/cleaned_data.csv\n")
'}]'}