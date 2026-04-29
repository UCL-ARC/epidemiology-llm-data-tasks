# Load required packages
library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define input and output paths
input_dir <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

# Function to recode NS-SEC missing values
recode_missing <- function(x) {
  x_num <- as.numeric(x)
  
  # Recode -999 to -2 (Script error/information lost)
  x_num[x_num == -999] <- -2
  
  # Recode -94 to -8 (Don't know/insufficient information)
  x_num[x_num == -94] <- -8
  
  # Recode -99, -98, and NA to -3 (Not asked/not interviewed)
  x_num[x_num %in% c(-99, -98) | is.na(x_num)] <- -3
  
  return(x_num)
}

# Function to recode to integer part (collapse subcategories)
recode_categorical <- function(x) {
  x_num <- recode_missing(x)
  
  # Take integer part
  x_int <- floor(x_num)
  
  # Only keep values 1-17
  x_int[x_int < 1 | x_int > 17] <- NA
  
  return(x_int)
}

# Function to create labels
create_labels <- function() {
  labels <- c(
    "-3" = "Not asked/not interviewed",
    "-2" = "Script error/information lost",
    "-1" = "Not applicable",
    "-9" = "Refusal",
    "-8" = "Don't know",
    "-7" = "Prefer not to say",
    "1" = "Employers in large organisations",
    "2" = "Higher managerial occupations",
    "3" = "Higher professional occupations",
    "4" = "Lower professional occupations",
    "5" = "Lower managerial occupations",
    "6" = "Higher supervisory occupations",
    "7" = "Intermediate occupations",
    "8" = "Employers in small orgs",
    "9" = "Own account workers",
    "10" = "Lower supervisory occupations",
    "11" = "Lower technical craft",
    "12" = "Semi routine occupations",
    "13" = "Routine occupations",
    "14" = "Never worked / Long-term unemployed",
    "15" = "Full-time students",
    "16" = "Not classified or inadequately stated",
    "17" = "Not classifiable for other reasons"
  )
  return(labels)
}

# Define waves and their variables
waves <- list(
  wave1 = list(
    file = "wave_one_lsype_family_background_2020.tab",
    mum = "W1nsseccatmum",
    dad = "W1nsseccatdad"
  ),
  wave2 = list(
    file = "wave_two_lsype_family_background_2020.tab",
    mum = "W2nsseccatmum",
    dad = "W2nsseccatdad"
  ),
  wave3 = list(
    file = "wave_three_lsype_family_background_2020.tab",
    mum = "W3cnsseccatmum",
    dad = "W3cnsseccatdad"
  ),
  wave4 = list(
    file = "wave_four_lsype_family_background_2020.tab",
    mum = "w4cnsseccatmum",
    dad = "w4cnsseccatdad"
  ),
  wave5 = list(
    file = "wave_five_lsype_family_background_2020.tab",
    mum = "w5Cnsseccatmum",
    dad = "w5Cnsseccatdad"
  )
)

# Define wave numbers
wave_numbers <- c(14, 15, 16, 17, 18)

# Process each wave
all_waves <- list()

for (i in seq_along(waves)) {
  wave <- waves[[i]]
  
  # Load the file - use paste0 for string concatenation
  df <- read_delim(paste0(input_dir, wave$file), delim = "\t")
  
  # Select only needed variables
  df <- df %>% select(NSID, !!sym(wave$mum), !!sym(wave$dad))
  
  # Recode
  df[[wave$mum]] <- recode_categorical(df[[wave$mum]])
  df[[wave$dad]] <- recode_categorical(df[[wave$dad]])
  
  # Create new data frame with standard names
  new_df <- df %>% select(NSID, !!sym(wave$mum), !!sym(wave$dad))
  new_df[[paste0("nssecma", wave_numbers[i])]] <- recode_categorical(new_df[[wave$mum]])
  new_df[[paste0("nssecpa", wave_numbers[i])]] <- recode_categorical(new_df[[wave$dad]])
  
  # Select only these columns
  new_df <- new_df %>% select(NSID, !!sym(paste0("nssecma", wave_numbers[i])), !!sym(paste0("nssecpa", wave_numbers[i])))
  
  all_waves[[i]] <- new_df
}

# Combine all waves
combined <- all_waves[[1]]
for (i in 2:length(all_waves)) {
  combined <- full_join(combined, all_waves[[i]], by = "NSID")
}

# Convert to labelled factors
labels <- create_labels()

for (wave_num in wave_numbers) {
  combined[[paste0("nssecma", wave_num)]] <- factor(combined[[paste0("nssecma", wave_num)]], 
                                                      levels = as.character(labels),
                                                      labels = labels)
  combined[[paste0("nssecpa", wave_num)]] <- factor(combined[[paste0("nssecpa", wave_num)]], 
                                                      levels = as.character(labels),
                                                      labels = labels)
}

# Select only required variables
combined <- combined %>% select(NSID, nssecma14:nssecpa18)

# Write to CSV
write_csv(combined, output_file)

cat("Data cleaning complete. Output written to:", output_file, "\n")