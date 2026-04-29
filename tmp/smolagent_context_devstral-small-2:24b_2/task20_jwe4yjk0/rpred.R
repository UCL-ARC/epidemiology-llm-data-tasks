library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)
library(stringr)

# Load each dataset into a separate object and merge using full_join by the ID variable
# The input files are tab-delimited (.tab) text files. Use `readr::read_delim()` with `delim = "\t"` to load them.

# Load each file explicitly by name from the metadata
wave_one <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t")
wave_two <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")
wave_three <- read_delim("data/input/wave_three_lsype_young_person_2020.tab", delim = "\t")
wave_four <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t")
wave_six <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t")
wave_seven <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t")
ns8_self_completion <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t")
ns9_main_interview <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t")

# Select NSID and the alcohol-related variables listed in the metadata
wave_one <- wave_one %>% select(NSID, W1alceverYP, W1alcmonYP, W1alcfreqYP)
wave_two <- wave_two %>% select(NSID, W2alceverYP, W2alcfreqYP)
wave_three <- wave_three %>% select(NSID, W3alceverYP, W3alcfreqYP)
wave_four <- wave_four %>% select(NSID, W4AlcEverYP, W4AlcFreqYP)
wave_six <- wave_six %>% select(NSID, W6AlcEverYP, W6AlcFreqYP)
wave_seven <- wave_seven %>% select(NSID, W7AlcEverYP, W7AlcFreqYP)
ns8_self_completion <- ns8_self_completion %>% select(NSID, W8AUDIT1, W8AUDIT2, W8AUDIT6)
ns9_main_interview <- ns9_main_interview %>% select(NSID, W9AUDIT1, W9AUDIT2, W9AUDIT3)

# Rename variables with short descriptive names and sweep/age suffixes upon loading
wave_one <- wave_one %>% rename(alcever14 = W1alceverYP, alcmon14 = W1alcmonYP, alcfreq14 = W1alcfreqYP)
wave_two <- wave_two %>% rename(alcever15 = W2alceverYP, alcfreq15 = W2alcfreqYP)
wave_three <- wave_three %>% rename(alcever16 = W3alceverYP, alcfreq16 = W3alcfreqYP)
wave_four <- wave_four %>% rename(alcever17 = W4AlcEverYP, alcfreq17 = W4AlcFreqYP)
wave_six <- wave_six %>% rename(alcever19 = W6AlcEverYP, alcfreq19 = W6AlcFreqYP)
wave_seven <- wave_seven %>% rename(alcever20 = W7AlcEverYP, alcfreq20 = W7AlcFreqYP)
ns8_self_completion <- ns8_self_completion %>% rename(AUDIT1_25 = W8AUDIT1, AUDIT2_25 = W8AUDIT2, AUDIT6_25 = W8AUDIT6)
ns9_main_interview <- ns9_main_interview %>% rename(AUDIT1_32 = W9AUDIT1, AUDIT2_32 = W9AUDIT2, AUDIT3_32 = W9AUDIT3)

# Merge all datasets using full_join by the ID variable
merged_data <- wave_one %>% 
  full_join(wave_two, by = "NSID") %>% 
  full_join(wave_three, by = "NSID") %>% 
  full_join(wave_four, by = "NSID") %>% 
  full_join(wave_six, by = "NSID") %>% 
  full_join(wave_seven, by = "NSID") %>% 
  full_join(ns8_self_completion, by = "NSID") %>% 
  full_join(ns9_main_interview, by = "NSID")

# Ensure all variables are present and handle missing values
merged_data <- merged_data %>% 
  mutate(across(c(alcever14, alcmon14, alcever15, alcever16, alcever17, alcever19, alcever20, AUDIT1_25, AUDIT1_32), ~ ifelse(is.na(.x), -999, .x)))

# Create a function to determine if a person is a known drinker at a given age
is_drinker <- function(age, data) {
  if (age == 14) {
    return(data$alcever14 == 1 & data$alcmon14 == 1)
  } else if (age == 15) {
    return(data$alcever15 == 1)
  } else if (age == 16) {
    return(data$alcever16 == 1)
  } else if (age == 17) {
    return(data$alcever17 == 1)
  } else if (age == 19) {
    return(data$alcever19 == 1)
  } else if (age == 20) {
    return(data$alcever20 == 1)
  } else if (age == 25) {
    return(data$AUDIT1_25 != 1)
  } else if (age == 32) {
    return(data$AUDIT1_32 != 1)
  }
}

# Create a function to determine the first age of alcohol consumption
first_age_of_alcohol_consumption <- function(data) {
  ages <- c(14, 15, 16, 17, 19, 20, 25, 32)
  first_age <- NA
  
  for (age in ages) {
    if (is_drinker(age, data)) {
      first_age <- age
      break
    }
  }
  
  if (!is.na(first_age)) {
    return(first_age)
  } else {
    # Check if the person is a confirmed never drinker
    never_drinker <- TRUE
    for (age in ages) {
      if (age == 14) {
        if (data$alcever14 == -999 | data$alcmon14 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$alcever14 != 2 | data$alcmon14 != 2) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 15) {
        if (data$alcever15 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$alcever15 != 2) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 16) {
        if (data$alcever16 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$alcever16 != 2) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 17) {
        if (data$alcever17 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$alcever17 != 2) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 19) {
        if (data$alcever19 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$alcever19 != 2) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 20) {
        if (data$alcever20 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$alcever20 != 2) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 25) {
        if (data$AUDIT1_25 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$AUDIT1_25 != 1) {
          never_drinker <- FALSE
          break
        }
      } else if (age == 32) {
        if (data$AUDIT1_32 == -999) {
          never_drinker <- FALSE
          break
        } else if (data$AUDIT1_32 != 1) {
          never_drinker <- FALSE
          break
        }
      }
    }
    
    if (never_drinker) {
      return(99)
    } else {
      return(-8)
    }
  }
}

# Apply the function to each row of the merged data
merged_data <- merged_data %>% 
  mutate(alcfst = pmap_dbl(list(alcever14, alcmon14, alcever15, alcever16, alcever17, alcever19, alcever20, AUDIT1_25, AUDIT1_32), 
                     ~ first_age_of_alcohol_consumption(data.frame(alcever14 = ..1, alcmon14 = ..2, alcever15 = ..3, alcever16 = ..4, alcever17 = ..5, alcever19 = ..6, alcever20 = ..7, AUDIT1_25 = ..8, AUDIT1_32 = ..9))))

# Convert alcfst to a factor with levels for each observed age (14, 15, 16, 17, 19, 20, 25, 32), plus 99 = "Never had alcohol" and -8 = "Don't know/insufficient information".
merged_data <- merged_data %>% 
  mutate(alcfst = factor(alcfst, levels = c(14, 15, 16, 17, 19, 20, 25, 32, 99, -8), labels = c("14", "15", "16", "17", "19", "20", "25", "32", "Never had alcohol", "Don't know/insufficient information")))

# Output only the following variables: NSID, alcfst
output_data <- merged_data %>% select(NSID, alcfst)

# Write exactly one CSV file named data/output/cleaned_data.csv
write.csv(output_data, "data/output/cleaned_data.csv", row.names = FALSE)

# Verify the output
cat("Output file created successfully with", nrow(output_data), "rows and", ncol(output_data), "columns.\n")
cat("Variables in output:", paste(names(output_data), collapse = ", "), "\n")
cat("First few rows:\n")
print(head(output_data))
cat("\nFactor levels for alcfst:\n")
print(levels(output_data$alcfst))