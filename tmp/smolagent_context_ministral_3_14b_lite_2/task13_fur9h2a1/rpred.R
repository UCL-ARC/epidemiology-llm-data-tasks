
library(readr)
library(dplyr)
library(tidyr)

# Load all datasets
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t")
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t")
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t")
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t")
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t")

# Function to process NS-SEC data
process_nssec <- function(data, mother_var, father_var, age_suffix) {
  # Define a function to process a single variable
  process_var <- function(var) {
    var <- as.numeric(var)

    # Handle missing values
    var[var %in% c(-999, -999.0, -9999)] <- -2
    var[var %in% c(-99, -99.0)] <- -3
    var[var %in% c(-98, -98.0)] <- -1
    var[var %in% c(-97, -97.0)] <- -2
    var[var %in% c(-94, -94.0)] <- -8
    var[var %in% c(-92, -92.0)] <- -9
    var[var %in% c(-91, -91.0)] <- -1

    # Collapse to major categories
    var[var %in% c(3.1, 3.2, 3.3, 3.4, 3)] <- 3.0
    var[var %in% c(4.1, 4.2, 4.3, 4.4, 4)] <- 4.0
    var[var %in% c(7.1, 7.2, 7.3, 7.4, 7)] <- 7.0
    var[var %in% c(8.1, 8.2, 8)] <- 8.0
    var[var %in% c(9.1, 9.2, 9)] <- 9.0
    var[var %in% c(11.1, 11.2, 11)] <- 11.0
    var[var %in% c(12.1, 12.2, 12.3, 12.4, 12.5, 12.6, 12.7, 12)] <- 12.0
    var[var %in% c(13.1, 13.2, 13.3, 13.4, 13.5, 13)] <- 13.0
    var[var %in% c(14.1, 14.2, 14.3, 14)] <- 14.0

    # Handle any remaining unexpected values
    var[is.na(var) | var == 0 | var %% 1 != 0] <- -3

    # Convert to factor
    factor(
      var,
      levels = c(-9, -8, -7, -3, -2, -1, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0),
      labels = c(
        "-9" = "Refusal",
        "-8" = "Don't know / insufficient information",
        "-7" = "Prefer not to say",
        "-3" = "Not asked at the fieldwork stage / not interviewed",
        "-2" = "Schedule not applicable / script error / information lost",
        "-1" = "Item not applicable",
        "1.0" = "Employers in large organisations",
        "2.0" = "Higher managerial occupations",
        "3.0" = "Higher professional",
        "4.0" = "Lower professional",
        "5.0" = "Lower managerial occupations",
        "6.0" = "Higher supervisory occupations",
        "7.0" = "Intermediate occupations",
        "8.0" = "Employers in small organisations",
        "9.0" = "Own account workers",
        "10.0" = "Lower supervisory occupations",
        "11.0" = "Lower technical occupations",
        "12.0" = "Semi-routine occupations",
        "13.0" = "Routine occupations",
        "14.0" = "Economically inactive",
        "15.0" = "Full-time students",
        "16.0" = "Not classified or inadequately stated",
        "17.0" = "Not classifiable for other reasons"
      )
    )
  }

  # Process mother variable
  data[[paste0("nssecma", age_suffix)]] <- process_var(data[[mother_var]])

  # Process father variable
  data[[paste0("nssecpa", age_suffix)]] <- process_var(data[[father_var]])

  return(data)
}

# Process each wave
wave1 <- process_nssec(wave1, "W1nsseccatmum", "W1nsseccatdad", "14")
wave2 <- process_nssec(wave2, "W2nsseccatmum", "W2nsseccatdad", "15")
wave3 <- process_nssec(wave3, "W3cnsseccatmum", "W3cnsseccatdad", "16")
wave4 <- process_nssec(wave4, "w4cnsseccatmum", "w4cnsseccatdad", "17")
wave5 <- process_nssec(wave5, "w5Cnsseccatmum", "w5Cnsseccatdad", "18")

# Merge all waves
cleaned_data <- full_join(wave1, wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# Select only ID and derived variables
cleaned_data <- cleaned_data %>%
  select(NSID,
         nssecma14, nssecpa14,
         nssecma15, nssecpa15,
         nssecma16, nssecpa16,
         nssecma17, nssecpa17,
         nssecma18, nssecpa18)

# Write output
write_csv(cleaned_data, "data/output/cleaned_data.csv")
