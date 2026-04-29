library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# File paths
path_prefix <- "data/input/"

# Load datasets
# Note: Some files only have NSID, but must be loaded to preserve cohort frame
file_list <- c(
  "wave_one_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "ns8_2015_derived.tab",
  "ns9_2022_derived_variables.tab",
  "ns9_2022_main_interview.tab"
)

data_list <- map(file_list, ~read_delim(paste0(path_prefix, .x), delim = "\t", col_types = cols(.default = "c"))) 

# Convert numeric columns that were read as character (since we used col_types = "c" for safety)
# We will specifically convert variables we need

# Merge datasets
full_data <- data_list[[1]]
for (i in 2:length(data_list)) {
  full_data <- full_join(full_data, data_list[[i]], by = "NSID")
}

# Helper function for missing value mapping
map_missing <- function(val, labels_meta) {
  # This function will be used within mutate to handle values
  # Based on standard missing-value codes provided in the instructions
  # -9 = Refusal, -8 = DK, -7 = Prefer not to say, -3 = Not asked, -2 = Schedule not app, -1 = Item not app
  # The metadata for urbind and gor shows -94 as 'Insufficient information' -> -8
  # W8DGOR and W9DRGN show -9, -8, -1
  
  # This is a simplified mapping logic applied to the specific variables below
  return(val)
}

# Process Variables

# 1. regub15 (Urban/Rural Age 15) from wave_two_lsype_family_background_2020.tab -> urbind
# 2. regub16 (Urban/Rural Age 16) from wave_three_lsype_family_background_2020.tab -> urbind
# 3. regov15 (Gov Region Age 15) from wave_two_lsype_family_background_2020.tab -> gor
# 4. regov16 (Gov Region Age 16) from wave_three_lsype_family_background_2020.tab -> gor
# 5. regor25 (Gov Region Age 25) from ns8_2015_derived.tab -> W8DGOR
# 6. regor32 (Gov Region Age 32) from ns9_2022_derived_variables.tab -> W9DRGN
# 7. regint32 (Nation Age 32) from ns9_2022_main_interview.tab -> W9NATIONRES

# We need to handle column name collisions from the merge (e.g., urbind.x, urbind.y)
# Since I used full_join on the whole list, and read_delim didn't know column names,
# let's refine the loading and merging to keep track of which variable comes from where.

# Re-loading with specific selection to avoid collision and ensure types
# Using a more robust merge approach

# Age 15
data_15 <- read_delim(paste0(path_prefix, "wave_two_lsype_family_background_2020.tab"), delim = "\t") %>%
  select(NSID, regub15 = urbind, regov15 = gor)

# Age 16
data_16 <- read_delim(paste0(path_prefix, "wave_three_lsype_family_background_2020.tab"), delim = "\t") %>%
  select(NSID, regub16 = urbind, regov16 = gor)

# Age 25 (Wave 8)
data_25 <- read_delim(paste0(path_prefix, "ns8_2015_derived.tab"), delim = "\t") %>%
  select(NSID, regor25 = W8DGOR)

# Age 32 (Wave 9)
data_32_der <- read_delim(paste0(path_prefix, "ns9_2022_derived_variables.tab"), delim = "\t") %>%
  select(NSID, regor32 = W9DRGN)

data_32_main <- read_delim(paste0(path_prefix, "ns9_2022_main_interview.tab"), delim = "\t") %>%
  select(NSID, regint32 = W9NATIONRES)

# Cohort Frame (Wave 1)
data_frame <- read_delim(paste0(path_prefix, "wave_one_lsype_young_person_2020.tab"), delim = "\t") %>%
  select(NSID)

# Final Merge
final_df <- data_frame %>%
  full_join(data_15, by = "NSID") %>%
  full_join(data_16, by = "NSID") %>%
  full_join(data_25, by = "NSID") %>%
  full_join(data_32_der, by = "NSID") %>%
  full_join(data_32_main, by = "NSID")

# Harmonization of missing values
# Standard: -9 Refusal, -8 DK, -7 Prefer not, -3 Not asked, -2 Schedule/Script, -1 Not app

# Function to apply missing value mapping based on labels
clean_var <- function(x) {
  x <- as.numeric(x)
  # General mapping based on metadata labels
  # For urbind/gor: -94 is "Insufficient information" -> -8
  # For W8DGOR/W9DRGN: -9=Refused, -8=Insuff Info, -1=Not app
  # For W9NATIONRES: -9=Refused, -8=DK, -3=Not asked, -1=Not app
  
  x <- ifelse(x == -94, -8, x)
  x[is.na(x)] <- -3
  return(x)
}

# Apply cleaning to all target variables
vars_to_clean <- c("regub15", "regub16", "regov15", "regov16", "regor25", "regor32", "regint32")
final_df <- final_df %>%
  mutate(across(all_of(vars_to_clean), clean_var))

# Set Labels as factors
# regub (Urban/Rural)
urb_labels <- c("1" = "Urban >= 10k - sparse", "2" = "Town & Fringe - sparse", "3" = "Village - sparse", "4" = "Hamlet and Isolated Dwelling - sparse", "5" = "Urban >= 10k - less sparse", "6" = "Town & Fringe - less sparse", "7" = "Village - less sparse", "8" = "Hamlet & Isolated Dwelling", "-8" = "Insufficient information", "-3" = "Not asked")

# regov/regor (Gov Region)
gov_labels <- c("1" = "North East", "2" = "North West", "3" = "Yorkshire and the Humber", "4" = "East Midlands", "5" = "West Midlands", "6" = "East of England", "7" = "London", "8" = "South East", "9" = "South West", "10" = "Wales", "11" = "Scotland", "12" = "Northern Ireland", "13" = "Unknown", "-9" = "Refused", "-8" = "Insufficient information", "-1" = "Not applicable", "-3" = "Not asked")

# regint (Nation)
nation_labels <- c("1" = "England", "2" = "Scotland", "3" = "Wales", "4" = "Northern Ireland", "5" = "Outside of UK or unknown", "-9" = "Refused", "-8" = "Don't know", "-3" = "Not asked", "-1" = "Not applicable")

# Apply factor labels
final_df <- final_df %>%
  mutate(
    regub15 = factor(regub15, levels = as.numeric(names(urb_labels)), labels = urb_labels),
    regub16 = factor(regub16, levels = as.numeric(names(urb_labels)), labels = urb_labels),
    regov15 = factor(regov15, levels = as.numeric(names(gov_labels)), labels = gov_labels),
    regov16 = factor(regov16, levels = as.numeric(names(gov_labels)), labels = gov_labels),
    regor25 = factor(regor25, levels = as.numeric(names(gov_labels)), labels = gov_labels),
    regor32 = factor(regor32, levels = as.numeric(names(gov_labels)), labels = gov_labels),
    regint32 = factor(regint32, levels = as.numeric(names(nation_labels)), labels = nation_labels)
  )

# Save to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
