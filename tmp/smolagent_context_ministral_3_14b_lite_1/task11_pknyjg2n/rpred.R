# Load required libraries
library(dplyr)
library(readr)

# Define file paths
file_paths <- c(
  "data/input/wave_one_lsype_family_background_2020.tab",
  "data/input/wave_two_lsype_family_background_2020.tab",
  "data/input/wave_three_lsype_family_background_2020.tab",
  "data/input/wave_four_lsype_family_background_2020.tab"
)

# Load datasets
wave1 <- read_delim(file_paths[1], delim = "\t")
wave2 <- read_delim(file_paths[2], delim = "\t")
wave3 <- read_delim(file_paths[3], delim = "\t")
wave4 <- read_delim(file_paths[4], delim = "\t")

# Function to harmonize missing values
harmonize <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3  # Default for NA values
  x[x == -999] <- -2
  x[x == -996] <- -2
  x[x == -96] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -1
  x[x == -94] <- -8
  x[x == -92] <- -9
  return(x)
}

# Process mother's economic activity variables
ecoactma14 <- wave1 %>% select(NSID, W1empsmum) %>% rename(ecoactma14 = W1empsmum) %>% 
  mutate(ecoactma14 = harmonize(ecoactma14))
  print(dim(ecoactma14))

ecoactma15 <- wave2 %>% select(NSID, W2empsmum) %>% rename(ecoactma15 = W2empsmum) %>% 
  mutate(ecoactma15 = harmonize(ecoactma15))
  print(dim(ecoactma15))

ecoactma16 <- wave3 %>% select(NSID, W3empsmum) %>% rename(ecoactma16 = W3empsmum) %>% 
  mutate(ecoactma16 = harmonize(ecoactma16))
  print(dim(ecoactma16))

ecoactma17 <- wave4 %>% select(NSID, w4empsmum) %>% rename(ecoactma17 = w4empsmum) %>% 
  mutate(ecoactma17 = harmonize(ecoactma17))
  print(dim(ecoactma17))

# Process father's economic activity variables
ecoactpa14 <- wave1 %>% select(NSID, W1empsdad) %>% rename(ecoactpa14 = W1empsdad) %>% 
  mutate(ecoactpa14 = harmonize(ecoactpa14))
  print(dim(ecoactpa14))

ecoactpa15 <- wave2 %>% select(NSID, W2empsdad) %>% rename(ecoactpa15 = W2empsdad) %>% 
  mutate(ecoactpa15 = harmonize(ecoactpa15))
  print(dim(ecoactpa15))

ecoactpa16 <- wave3 %>% select(NSID, W3empsdad) %>% rename(ecoactpa16 = W3empsdad) %>% 
  mutate(ecoactpa16 = harmonize(ecoactpa16))
  print(dim(ecoactpa16))

ecoactpa17 <- wave4 %>% select(NSID, w4empsdad) %>% rename(ecoactpa17 = w4empsdad) %>% 
  mutate(ecoactpa17 = harmonize(ecoactpa17))
  print(dim(ecoactpa17))

# Merge all datasets
cleaned_data <- ecoactma14
cleaned_data <- left_join(cleaned_data, ecoactma15, by = "NSID")
cleaned_data <- left_join(cleaned_data, ecoactma16, by = "NSID")
cleaned_data <- left_join(cleaned_data, ecoactma17, by = "NSID")
cleaned_data <- left_join(cleaned_data, ecoactpa14, by = "NSID")
cleaned_data <- left_join(cleaned_data, ecoactpa15, by = "NSID")
cleaned_data <- left_join(cleaned_data, ecoactpa16, by = "NSID")
cleaned_data <- left_join(cleaned_data, ecoactpa17, by = "NSID")

print(dim(cleaned_data))

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output")
}

# Write the cleaned data to CSV
write_csv(cleaned_data, "data/output/cleaned_data.csv")
print("CSV file written successfully to data/output/cleaned_data.csv")