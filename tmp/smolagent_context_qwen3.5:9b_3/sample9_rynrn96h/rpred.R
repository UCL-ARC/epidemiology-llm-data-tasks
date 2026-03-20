library(haven)
library(dplyr)
library(purrr)
library(readr)

# Define file paths
wave1_file <- "data/input/wave_one_lsype_family_background_2020.tab"
wave2_file <- "data/input/wave_two_lsype_family_background_2020.tab"
wave4_file <- "data/input/wave_four_lsype_family_background_2020.tab"

# Load data
wave1_data <- read_delim(wave1_file, delim = "\t")
wave2_data <- read_delim(wave2_file, delim = "\t")
wave4_data <- read_delim(wave4_file, delim = "\t")

cat("Loaded wave 1:", nrow(wave1_data), "rows\n")
cat("Loaded wave 2:", nrow(wave2_data), "rows\n")
cat("Loaded wave 4:", nrow(wave4_data), "rows\n")

# Combine all data with wave identifiers
wave1_data <- wave1_data %>% 
  mutate(wave = 1) %>%
  select(NSID, w1mum = W1hiqualmum, w1dad = W1hiqualdad)

wave2_data <- wave2_data %>% 
  mutate(wave = 2) %>%
  select(NSID, w2mum = W2hiqualmum, w2dad = W2hiqualdad)

wave4_data <- wave4_data %>% 
  mutate(wave = 4) %>%
  select(NSID, w4mum = w4hiqualmum, w4dad = w4hiqualdad)

# Full join all waves
cat("Joining data waves...\n")
combined_data <- full_join(wave1_data, wave2_data, by = "NSID")
combined_data <- full_join(combined_data, wave4_data, by = "NSID")

cat("Combined data has:", nrow(combined_data), "rows\n")
cat("Variables:", names(combined_data), "\n")

# Define value labels for detailed education
labels_detailed <- c(
  "-999.0" = "Missing - household data lost",
  "-99.0" = "Not interviewed",
  "-98.0" = "Not present",
  "-94.0" = "Insufficient information",
  "-92.0" = "Refused",
  "-91.0" = "Not applicable",
  "-1.0" = "Don\'t know",
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
  "20.0" = "No qualification mentioned"
)

# W1/W2 mappings
w1w2_mum_mapping <- c(
  "-999.0" = "-3",
  "-99.0" = "-1",
  "-98.0" = "-1",
  "-94.0" = "-3",
  "-92.0" = "-9",
  "-91.0" = "-1",
  "-1.0" = "-8"
)

w1w2_dad_mapping <- w1w2_mum_mapping

# W4 mappings
w4_mum_mapping <- c(
  "-99.0" = "-1",
  "-98.0" = "-1",
  "-94.0" = "-3"
)

w4_dad_mapping <- w4_mum_mapping

# Consolidate and harmonize maternal education
cat("Processing maternal education...\n")
n <- nrow(combined_data)
harmonized_dtlma <- numeric(n)
wave_of_maternal <- integer(n)

for (i in seq_len(n)) {
  val_w1 <- combined_data$w1mum[i]
  val_w2 <- combined_data$w2mum[i]
  val_w4 <- combined_data$w4mum[i]
  
  if (!is.na(val_w1)) {
    harmonized_dtlma[i] <- val_w1
    wave_of_maternal[i] <- 1
  } else if (!is.na(val_w2)) {
    harmonized_dtlma[i] <- val_w2
    wave_of_maternal[i] <- 2
  } else if (!is.na(val_w4)) {
    harmonized_dtlma[i] <- val_w4
    wave_of_maternal[i] <- 4
  } else {
    harmonized_dtlma[i] <- NA
  }
}

# Apply mapping based on wave
harmonized_dtlma_char <- as.character(harmonized_dtlma)
for (i in seq_len(n)) {
  if (!is.na(harmonized_dtlma[i])) {
    if (wave_of_maternal[i] %in% c(1, 2)) {
      for (old_code in names(w1w2_mum_mapping)) {
        if (harmonized_dtlma[i] == old_code) {
          harmonized_dtlma_char[i] <- w1w2_mum_mapping[old_code]
          break
        }
      }
    } else if (wave_of_maternal[i] == 4) {
      for (old_code in names(w4_mum_mapping)) {
        if (harmonized_dtlma[i] == old_code) {
          harmonized_dtlma_char[i] <- w4_mum_mapping[old_code]
          break
        }
      }
    }
  }
}

harmonized_dtlma_char[is.na(harmonized_dtlma)] <- "-3"
harmonized_dtlma <- as.numeric(harmonized_dtlma_char)

# Store in dataframe
combined_data$educdtlma <- harmonized_dtlma

# Consolidate and harmonize paternal education
cat("Processing paternal education...\n")
harmonized_dtlpa <- numeric(n)
wave_of_paternal <- integer(n)

for (i in seq_len(n)) {
  val_w1 <- combined_data$w1dad[i]
  val_w2 <- combined_data$w2dad[i]
  val_w4 <- combined_data$w4dad[i]
  
  if (!is.na(val_w1)) {
    harmonized_dtlpa[i] <- val_w1
    wave_of_paternal[i] <- 1
  } else if (!is.na(val_w2)) {
    harmonized_dtlpa[i] <- val_w2
    wave_of_paternal[i] <- 2
  } else if (!is.na(val_w4)) {
    harmonized_dtlpa[i] <- val_w4
    wave_of_paternal[i] <- 4
  } else {
    harmonized_dtlpa[i] <- NA
  }
}

# Apply mapping based on wave
harmonized_dtlpa_char <- as.character(harmonized_dtlpa)
for (i in seq_len(n)) {
  if (!is.na(harmonized_dtlpa[i])) {
    if (wave_of_paternal[i] %in% c(1, 2)) {
      for (old_code in names(w1w2_dad_mapping)) {
        if (harmonized_dtlpa[i] == old_code) {
          harmonized_dtlpa_char[i] <- w1w2_dad_mapping[old_code]
          break
        }
      }
    } else if (wave_of_paternal[i] == 4) {
      for (old_code in names(w4_dad_mapping)) {
        if (harmonized_dtlpa[i] == old_code) {
          harmonized_dtlpa_char[i] <- w4_dad_mapping[old_code]
          break
        }
      }
    }
  }
}

harmonized_dtlpa_char[is.na(harmonized_dtlpa)] <- "-3"
harmonized_dtlpa <- as.numeric(harmonized_dtlpa_char)

# Store in dataframe
combined_data$educdtlpa <- harmonized_dtlpa

# Create educdtlma factor with labels
cat("Creating educdtlma factor...\n")
level_names_dtlma <- sort(unique(c(as.numeric(names(labels_detailed)), combined_data$educdtlma)))
educdtlma <- factor(combined_data$educdtlma, levels = level_names_dtlma, labels = labels_detailed)
combined_data$educdtlma <- educdtlma

# Create educdtlpa factor with labels
cat("Creating educdtlpa factor...\n")
level_names_dtlpa <- sort(unique(c(as.numeric(names(labels_detailed)), combined_data$educdtlpa)))
educdtlpa <- factor(combined_data$educdtlpa, levels = level_names_dtlpa, labels = labels_detailed)
combined_data$educdtlpa <- educdtlpa

# Function to map to collapsed
class_to_collapsed <- function(val) {
  if (is.na(val)) return(NA_real_)
  if (val %in% c(1L, 2L, 3L, 4L)) return(0L)
  if (val %in% c(5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L)) return(1L)
  if (val == 18L) return(2L)
  if (val == 19L) return(3L)
  if (val == 20L) return(4L)
  return(NA_real_)
}

# Create collapsed maternal education (educma)
cat("Creating collapsed maternal education (educma)...\n")
educma_raw <- sapply(combined_data$educdtlma, class_to_collapsed)

labels_educma <- c(
  "-9" = "Refusal",
  "-8" = "Don\'t know/insufficient information",
  "-3" = "Not asked at the fieldwork stage/participated/interviewed",
  "-2" = "Schedule not applicable/Script error/information lost",
  "-1" = "Item not applicable",
  "0" = "NVQ 4-5: degree-level qualifications and above",
  "1" = "NVQ 1-3: sub-degree qualifications",
  "2" = "None/entry: training programmes below NVQ level",
  "3" = "Other: qualifications where the level is unspecified",
  "4" = "No qualifications mentioned"
)

# Convert to factor
all_levels_educma <- sort(unique(c(educma_raw, -9, -8, -3, -2, -1)))
combined_data$educma <- factor(educma_raw, levels = all_levels_educma, labels = labels_educma[as.character(all_levels_educma)])

# Create collapsed paternal education (educpa)
cat("Creating collapsed paternal education (educpa)...\n")
educpa_raw <- sapply(combined_data$educdtlpa, class_to_collapsed)

combined_data$educpa <- factor(educpa_raw, levels = all_levels_educma, labels = labels_educma[as.character(all_levels_educma)])

# Debug: print column names
cat("Final variables before select:", names(combined_data), "\n")

# Select only the required variables
cat("Selecting final variables...\n")
final_data <- combined_data %>%
  select(NSID, educdtlma, educdtlpa, educma, educpa)

cat("Final data has:", nrow(final_data), "rows\n")
cat("Variables:", names(final_data), "\n")

# Write to CSV
write_csv(final_data, "data/output/cleaned_data.csv")

cat("Successfully wrote data to data/output/cleaned_data.csv\n")
cat("Done!\n")'}]'}]```json
{
  