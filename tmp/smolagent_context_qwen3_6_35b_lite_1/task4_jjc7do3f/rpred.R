library(dplyr)
library(readr)
library(haven)
library(labelled)

# Ensure output directory exists
dir.create("data/output", showWarnings = FALSE)

# Load all files from the metadata
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w6 <- read_delim("data/input/wave_six_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w7 <- read_delim("data/input/wave_seven_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
w8 <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
w9 <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Merge all files by NSID
df <- w1 %>%
  full_join(w4, by = "NSID") %>%
  full_join(w6, by = "NSID") %>%
  full_join(w7, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9, by = "NSID")

cat("Merged data frame dimensions:", nrow(df), "rows,", ncol(df), "columns\n")

# Function to harmonize sexual orientation variables
harmonize_sori <- function(x) {
  x <- as.numeric(x)
  # Map original codes to standard codes based on metadata labels
  x[x == -999] <- -2   # Schedule not applicable / declined self completion
  x[x == -998] <- -2   # Schedule not applicable
  x[x == -997] <- -2   # Information lost
  x[x == -995] <- -2   # Information lost
  x[x == -99] <- -3    # Not asked at fieldwork stage
  x[x == -100] <- -2   # Respondent declined sexual experience questions
  x[x == -97] <- -2    # Respondent declined self completion / Refused self completion
  x[x == -94] <- -8    # Don't know / insufficient information
  x[x == -92] <- -9    # Refused
  x[x == -91] <- -1    # Not applicable
  x[x == -1] <- -8     # Don't know
  x[x == -8] <- -8     # Don't know
  x[x == -9] <- -9     # Refused
  x[x == -3] <- -3     # Not asked at fieldwork stage
  # Map "Prefer not to say" (code 5) to -7
  x[x == 5] <- -7
  return(x)
}

# Create harmonized variables for each wave
# Age 19 (Wave 6)
w6_data <- w6 %>% select(NSID, W6SexualityYP)
w6_data$W6SexualityYP <- harmonize_sori(w6_data$W6SexualityYP)

# Age 20 (Wave 7)
w7_data <- w7 %>% select(NSID, W7SexualityYP)
w7_data$W7SexualityYP <- harmonize_sori(w7_data$W7SexualityYP)

# Age 25 (Wave 8)
w8_data <- w8 %>% select(NSID, W8SEXUALITY)
w8_data$W8SEXUALITY <- harmonize_sori(w8_data$W8SEXUALITY)

# Age 32 (Wave 9)
w9_data <- w9 %>% select(NSID, W9SORI)
w9_data$W9SORI <- harmonize_sori(w9_data$W9SORI)

# Create labelled factors with consistent categories and labels
create_sori_factor <- function(x) {
  factor(x, levels = c(1, 2, 3, 4, -7, -8, -9, -1, -2, -3),
         labels = c("Heterosexual / Straight", "Gay / Lesbian", "Bisexual", "Other",
                    "Prefer not to say", "Don't know", "Refused", "Not applicable",
                    "Schedule not applicable", "Not asked at fieldwork"))
}

# Add harmonized variables to df by joining
result <- df %>%
  select(NSID) %>%
  left_join(w6_data %>% select(NSID, sori19 = W6SexualityYP), by = "NSID") %>%
  left_join(w7_data %>% select(NSID, sori20 = W7SexualityYP), by = "NSID") %>%
  left_join(w8_data %>% select(NSID, sori25 = W8SEXUALITY), by = "NSID") %>%
  left_join(w9_data %>% select(NSID, sori32 = W9SORI), by = "NSID")

# Convert to labelled factors
result$sori19 <- create_sori_factor(result$sori19)
result$sori20 <- create_sori_factor(result$sori20)
result$sori25 <- create_sori_factor(result$sori25)
result$sori32 <- create_sori_factor(result$sori32)

# Write output
write_csv(result, "data/output/cleaned_data.csv")

cat("Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(result), "\n")
cat("Columns:", names(result), "\n")

# Print summary of each variable
print(summary(result$sori19))
print(summary(result$sori20))
print(summary(result$sori25))
print(summary(result$sori32))
