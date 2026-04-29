library(haven)
library(dplyr)
library(readr)

# Load all datasets
wave1 <- readr::read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave1 <- wave1 %>% select(NSID, W1alceverYP, W1alcmonYP) %>% rename(alcever_14 = W1alceverYP, alcmon_14 = W1alcmonYP)

wave2 <- readr::read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave2 <- wave2 %>% select(NSID, W2alceverYP) %>% rename(alcever_15 = W2alceverYP)

wave3 <- readr::read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave3 <- wave3 %>% select(NSID, W3alceverYP) %>% rename(alcever_16 = W3alceverYP)

wave4 <- readr::read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave4 <- wave4 %>% select(NSID, W4AlcEverYP) %>% rename(alcever_17 = W4AlcEverYP)

wave6 <- readr::read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave6 <- wave6 %>% select(NSID, W6AlcEverYP) %>% rename(alcever_19 = W6AlcEverYP)

wave7 <- readr::read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
wave7 <- wave7 %>% select(NSID, W7AlcEverYP) %>% rename(alcever_20 = W7AlcEverYP)

wave8 <- readr::read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
wave8 <- wave8 %>% select(NSID, W8AUDIT1) %>% rename(audit_freq_25 = W8AUDIT1)

wave9 <- readr::read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")
wave9 <- wave9 %>% select(NSID, W9AUDIT1) %>% rename(audit_freq_32 = W9AUDIT1)

# Merge all datasets
merged_data <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave6, by = "NSID") %>%
  full_join(wave7, by = "NSID") %>%
  full_join(wave8, by = "NSID") %>%
  full_join(wave9, by = "NSID")

# Function to determine if a person is a drinker at a given age
is_drinker <- function(data, age) {
  if (age == 14) {
    data["alcever_14"] == 1 & data["alcmon_14"] == 1
  } else if (age %in% c(15, 16, 17, 19, 20)) {
    data[[paste0("alcever_", age)]] == 1
  } else if (age %in% c(25, 32)) {
    data[[paste0("audit_freq_", age)]] > 1
  } else {
    FALSE
  }
}

# Function to determine the earliest age of alcohol consumption
determine_alcfst <- function(row) {
  ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  drinker_status <- numeric(length(ages))

  for (i in seq_along(ages)) {
    age <- ages[i]
    if (age == 14) {
      drinker_status[i] <- !is.na(row["alcever_14"]) && !is.na(row["alcmon_14"]) && is_drinker(row, age)
    } else if (age %in% c(15, 16, 17, 19, 20)) {
      col_name <- paste0("alcever_", age)
      drinker_status[i] <- !is.na(row[[col_name]]) && is_drinker(row, age)
    } else {
      col_name <- paste0("audit_freq_", age)
      drinker_status[i] <- !is.na(row[[col_name]]) && is_drinker(row, age)
    }
  }

  # Find the earliest age where the person is a drinker
  valid_ages <- ages[drinker_status]
  if (length(valid_ages) > 0) {
    return(min(valid_ages))
  }

  # Check if the person is a confirmed never drinker
  all_na <- sapply(ages, function(age) {
    if (age == 14) {
      is.na(row["alcever_14"]) || is.na(row["alcmon_14"])
    } else if (age %in% c(15, 16, 17, 19, 20)) {
      is.na(row[[paste0("alcever_", age)]])
    } else {
      is.na(row[[paste0("audit_freq_", age)]])
    }
  })

  if (all(!drinker_status)) {
    return(99)  # Confirmed never drinker
  } else if (any(all_na)) {
    return(-8)  # Missing data
  }
}

# Apply the function to each row using a loop
alcfst_vector <- numeric(nrow(merged_data))
for (i in 1:nrow(merged_data)) {
  alcfst_vector[i] <- tryCatch({
    determine_alcfst(merged_data[i, ])
  }, error = function(e) {
    -8  # Default to unknown if there's an error
  })
}

# Add the alcfst column to the merged data
merged_data$alcfst <- alcfst_vector

# Convert alcfst to a factor with appropriate labels
alcfst_levels <- c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8)

alcfst_labels <- c("14", "15", "16", "17", "19", "20", "25", "32", 
                   "Never had alcohol", "Don't know/insufficient information")

merged_data$alcfst <- factor(merged_data$alcfst, 
                             levels = alcfst_levels, 
                             labels = alcfst_labels)

# Select only the required variables
final_data <- merged_data %>% 
  select(NSID, alcfst)

# Write the output to a CSV file
write.csv(final_data, "data/output/cleaned_data.csv", row.names = FALSE)