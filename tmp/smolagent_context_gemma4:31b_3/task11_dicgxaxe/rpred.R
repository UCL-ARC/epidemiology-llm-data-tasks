library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define missing value mappings based on metadata and general rules
# Standard: -9=Refusal, -8=Don't know, -1=N/A, -3=Not asked, -2=Script error, -7=Prefer not to say
# Meta mapping for employment variables:
# -999.0 (Lost/Missing household) -> -2 (Script error/info lost)
# -99.0 (Not interviewed) -> -3 (Not asked)
# -98.0 (Not present) -> -3 (Not asked)
# -94.0 (Insufficient info) -> -8 (Don't know/insufficient)
# -92.0 (Refusal) -> -9 (Refusal)
# -996.0 (No parent in household) -> -1 (N/A)

map_missing <- function(x) {
  x <- as.numeric(x)
  x[x == -999.0] <- -2
  x[x == -99.0]  <- -3
  x[x == -98.0]  <- -3
  x[x == -94.0]  <- -8
  x[x == -92.0]  <- -9
  x[x == -996.0] <- -1
  x[is.na(x)]    <- -3
  return(x)
}

# Define harmonization for parental employment
# Categories: 1=full-time, 2=part-time, 3=unemployed, 4=training, 5=education, 6=home, 7=retired, 8=sick/disabled, 9=other
harmonize_emp <- function(x) {
  val <- map_missing(x)
  # Since the labels in metadata are already aligned (1-9), we just map the missing values
  # and keep 1-9 as is.
  return(val)
}

# Load data
data1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
data2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
data3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
data4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))

# Merge datasets
df <- data1 %>%
  full_join(data2, by = "NSID") %>%
  full_join(data3, by = "NSID") %>%
  full_join(data4, by = "NSID")

# Process Parental Employment (Detailed variables)
# Age 14 (W1), 15 (W2), 16 (W3), 17 (W4)
# Mapping: W1empsmum -> ecoactdtma14, W1empsdad -> ecoactdtpa14, etc.

emp_labels <- c(
  "1" = "full-time", "2" = "part-time", "3" = "unemployed", 
  "4" = "training", "5" = "education", "6" = "home", 
  "7" = "retired", "8" = "sick/disabled", "9" = "other",
  "-9" = "Refusal", "-8" = "Don't know", "-1" = "Item not applicable",
  "-3" = "Not asked", "-2" = "Information lost", "-7" = "Prefer not to say"
)

process_emp_var <- function(var_name) {
  vals <- harmonize_emp(df[[var_name]])
  factor_vals <- factor(vals, levels = as.numeric(names(emp_labels)), labels = emp_labels)
  return(factor_vals)
}

df_final <- df %>%
  mutate(
    ecoactdtma14 = process_emp_var("W1empsmum"),
    ecoactdtpa14 = process_emp_var("W1empsdad"),
    ecoactdtma15 = process_emp_var("W2empsmum"),
    ecoactdtpa15 = process_emp_var("W2empsdad"),
    ecoactdtma16 = process_emp_var("W3empsmum"),
    ecoactdtpa16 = process_emp_var("W3empsdad"),
    ecoactdtma17 = process_emp_var("w4empsmum"),
    ecoactdtpa17 = process_emp_var("w4empsdad")
  )

# Select only ID and derived variables
df_final <- df_final %>%
  select(NSID, starts_with("ecoactdt"))

write_csv(df_final, "data/output/cleaned_data.csv")