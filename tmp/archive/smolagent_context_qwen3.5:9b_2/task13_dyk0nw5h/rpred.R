# Load required packages
library(readr)
library(dplyr)
library(haven)
library(purrr)
library(labelled)

# Set paths
input_dir <- "data/input/"
output_dir <- "data/output/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load the 5 wave files
wave1 <- read_delim(paste0(input_dir, "wave_one_lsype_family_background_2020.tab"), 
                    delim = "\t")
wave2 <- read_delim(paste0(input_dir, "wave_two_lsype_family_background_2020.tab"), 
                    delim = "\t")
wave3 <- read_delim(paste0(input_dir, "wave_three_lsype_family_background_2020.tab"), 
                    delim = "\t")
wave4 <- read_delim(paste0(input_dir, "wave_four_lsype_family_background_2020.tab"), 
                    delim = "\t")
wave5 <- read_delim(paste0(input_dir, "wave_five_lsype_family_background_2020.tab"), 
                    delim = "\t")

# Function to recode NS-SEC variables
recode_nssec <- function(x) {
  # Convert to numeric to handle character/NA
  x_num <- as.numeric(x)
  
  # Handle NA missing values
  x_num[is.na(x_num)] <- NA_real_
  
  # Recode -999 to -2 (Script error/information lost)
  x_num[x_num == -999] <- -2
  
  # Recode -94 to -8 (Don't know/insufficient information)
  x_num[x_num == -94] <- -8
  
  # Recode -99, -98 to -3 (Not asked/not interviewed)
  x_num[x_num == -99] <- -3
  x_num[x_num == -98] <- -3
  
  # Take integer part for fractional categories (collapse 3.1, 3.2 -> 3, etc.)
  x_int <- floor(abs(x_num[!is.na(x_num)]))
  
  # Only keep valid categories 1-17, recode others to NA for invalid categories
  valid <- x_int >= 1 & x_int <= 17
  x_int[!valid] <- NA_real_
  
  x_int
}

# Process each wave's NS-SEC variables
wave1_trans <- wave1 %>%
  mutate(nssecma14 = recode_nssec(W1nsseccatmum),
         nssecpa14 = recode_nssec(W1nsseccatdad)) %>%
  select(NSID, nssecma14, nssecpa14, everything())

wave2_trans <- wave2 %>%
  mutate(nssecma15 = recode_nssec(W2nsseccatmum),
         nssecpa15 = recode_nssec(W2nsseccatdad)) %>%
  select(NSID, nssecma15, nssecpa15, everything())

wave3_trans <- wave3 %>%
  mutate(nssecma16 = recode_nssec(W3cnsseccatmum),
         nssecpa16 = recode_nssec(W3cnsseccatdad)) %>%
  select(NSID, nssecma16, nssecpa16, everything())

wave4_trans <- wave4 %>%
  mutate(nssecma17 = recode_nssec(w4cnsseccatmum),
         nssecpa17 = recode_nssec(w4cnsseccatdad)) %>%
  select(NSID, nssecma17, nssecpa17, everything())

wave5_trans <- wave5 %>%
  mutate(nssecma18 = recode_nssec(w5Cnsseccatmum),
         nssecpa18 = recode_nssec(w5Cnsseccatdad)) %>%
  select(NSID, nssecma18, nssecpa18, everything())

# Merge all waves using NSID
final_data <- wave1_trans %>%
  full_join(wave2_trans, by = "NSID") %>%
  full_join(wave3_trans, by = "NSID") %>%
  full_join(wave4_trans, by = "NSID") %>%
  full_join(wave5_trans, by = "NSID")

# Labels for categories 1-17
cat_labels <- c(
  "Employers in large organisations",
  "Higher managerial occupations",
  "Higher professional traditional employee",
  "Higher professional new employee",
  "Higher professional traditional self emp",
  "Higher professional new self emp",
  "Lower professional traditional employee",
  "Lower professional new employee",
  "Lower professional traditional self emp",
  "Lower professional new self emp",
  "Lower managerial occupations",
  "Higher supervisory occupations",
  "Intermediate clerical and administrative",
  "Intermediate sales and service",
  "Intermediate technical and auxiliary",
  "Intermediate engineering",
  "Employers in small orgs non-professional"
)

# Missing value labels
miss_labels <- c(
  "Refusal",
  "Don't know",
  "Prefer not to say",
  "Not asked/not interviewed",
  "Script error/information lost",
  "Not applicable"
)

# Combine all labels (17 categories + 6 missing = 23 labels)
all_labels <- c(cat_labels, miss_labels)

# Create factor variables with labels for all waves
nssec_vars <- c("nssecma14", "nssecma15", "nssecma16", "nssecma17", "nssecma18",
                "nssecpa14", "nssecpa15", "nssecpa16", "nssecpa17", "nssecpa18")

for (v in nssec_vars) {
  final_data[[v]] <- factor(final_data[[v]],
                            labels = all_labels,
                            exclude = NA,
                            levels = c(1:17, -9, -8, -7, -3, -2, -1))
}

# Select only required variables
output_vars <- c("NSID", 
                 "nssecma14", "nssecpa14",
                 "nssecma15", "nssecpa15",
                 "nssecma16", "nssecpa16",
                 "nssecma17", "nssecpa17",
                 "nssecma18", "nssecpa18")

final_output <- final_data %>% select(all_of(output_vars))

# Write to CSV
write_csv(final_output, paste0(output_dir, "cleaned_data.csv"))

cat("Dataset cleaned and saved to", output_dir, "cleaned_data.csv", "\n")
