# Load required libraries
library(readr)
library(dplyr)
library(labelled)

# Define path prefixes
input_path  <- "data/input/"
output_path <- "data/output/"

# Helper functions to map missing values and collapse NS-SEC
map_missing <- function(x) {
  y <- ifelse(is.na(x), -3, x)  # NA → -3 by default
  y <- ifelse(y %in% c(-999.0, -998.0, -997.0, -995.0), -2,
            ifelse(y == -94.0, -8,
                    ifelse(y %in% c(-99.0, -98.0), -3,
                          ifelse(y == -1.0, -1,
                                ifelse(y < 0, -2, y)))))
  return(y)
}

major_nssec <- function(x) {
  y <- map_missing(x)
  ifelse(is.na(y), y, ifelse(y >= 1, floor(y), y))
}

# Read wave files
wave1 <- read_delim(paste0(input_path, "wave_one_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), progress = FALSE)
wave2 <- read_delim(paste0(input_path, "wave_two_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), progress = FALSE)
wave3 <- read_delim(paste0(input_path, "wave_three_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), progress = FALSE)
wave4 <- read_delim(paste0(input_path, "wave_four_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), progress = FALSE)
wave5 <- read_delim(paste0(input_path, "wave_five_lsype_family_background_2020.tab"), delim = "\t", col_types = cols(), progress = FALSE)

# Create target variables for each wave
wave1 <- wave1 %>% mutate(nssecma14 = major_nssec(W1nsseccatmum), nssecpa14 = major_nssec(W1nsseccatdad))
wave2 <- wave2 %>% mutate(nssecma15 = major_nssec(W2nsseccatmum), nssecpa15 = major_nssec(W2nsseccatdad))
wave3 <- wave3 %>% mutate(nssecma16 = major_nssec(W3cnsseccatmum), nssecpa16 = major_nssec(W3cnsseccatdad))
wave4 <- wave4 %>% mutate(nssecma17 = major_nssec(w4cnsseccatmum), nssecpa17 = major_nssec(w4cnsseccatdad))
wave5 <- wave5 %>% mutate(nssecma18 = major_nssec(w5Cnsseccatmum), nssecpa18 = major_nssec(w5Cnsseccatdad))

# Keep only ID and target variables
wave1_out <- wave1 %>% select(NSID, nssecma14, nssecpa14)
wave2_out <- wave2 %>% select(NSID, nssecma15, nssecpa15)
wave3_out <- wave3 %>% select(NSID, nssecma16, nssecpa16)
wave4_out <- wave4 %>% select(NSID, nssecma17, nssecpa17)
wave5_out <- wave5 %>% select(NSID, nssecma18, nssecpa18)

# Merge all waves by NSID
cleaned <- wave1_out %>%
  full_join(wave2_out, by = "NSID") %>%
  full_join(wave3_out, by = "NSID") %>%
  full_join(wave4_out, by = "NSID") %>%
  full_join(wave5_out, by = "NSID")

# Write output
write_csv(cleaned, file.path(output_path, "cleaned_data.csv"))