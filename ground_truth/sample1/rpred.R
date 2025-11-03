# Install necessary packages if not already installed
# install.packages(c("dplyr", "tidyr", "readr", "haven", "data.table"))

# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(data.table)

# Define a function to clean and preprocess a single wave's data
clean_wave_data <- function(file_path, wave_number) {
  tryCatch({

    # Construct the file path
    file_path <- paste0("data/input/", file_path)

    # Read the data using read_fwf (assuming tab-delimited)
    data <- read_fwf(file_path, col_names = FALSE, comment = "#")

    # Rename the sex variable consistently across waves.  Handle missing values.
    if(wave_number %in% c(1, 12)){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 2){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 3){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 4){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 5){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 6){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 7){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 8){
      data <- data %>% rename(sex = V1)
    } else if (wave_number == 9){
      data <- data %>% rename(sex = V1)
    }

    # Convert sex to a standardized format (1=Male, 2=Female, NA=Missing)
    data <- data %>%
      mutate(sex = case_when(
        sex %in% c("-99", "-92", "-91", "-998", "-997", "-995", "-1", "-9", "-8") ~ NA_character_,
        sex == "1" ~ 1,
        sex == "2" ~ 2,
        TRUE ~ NA_character_ 
      ))

    # Remove duplicate records based on NSID
    # data <- data %>% distinct(NSID, .keep_all = TRUE)

    return(data)

  }, error = function(e) {
    cat("Error processing file", file_path, ":", e$message, "\n")
    return(NULL)
  })


# List of wave files and their corresponding wave numbers
wave_files <- list(
  "wave_one_lsype_young_person_2020.tab",
  "wave_two_lsype_young_person_2020.tab",
  "wave_three_lsype_young_person_2020.tab",
  "wave_four_lsype_young_person_2020.tab",
  "wave_five_lsype_young_person_2020.tab",
  "wave_six_lsype_young_person_2020.tab",
  "wave_seven_lsype_young_person_2020.tab",
  "wave_eight_lsype_young_person_2020.tab",
  "wave_nine_lsype_young_person_2020.tab"
)

# Create an empty list to store the cleaned data
cleaned_data <- list()

# Iterate over the wave files and clean the data
for (i in 1:length(wave_files)) {
  file_path <- wave_files[[i]]
  wave_number <- i
  cleaned_data[[i]] <- clean_wave_data(file_path, wave_number)
}

# Combine the cleaned data into a single dataframe
combined_data <- do.call(rbind, cleaned_data)

# Print the first few rows of the combined data
print(head(combined_data))

# Save the combined data to a CSV file (optional)
# write.csv(combined_data, "combined_data.csv", row.names = FALSE)