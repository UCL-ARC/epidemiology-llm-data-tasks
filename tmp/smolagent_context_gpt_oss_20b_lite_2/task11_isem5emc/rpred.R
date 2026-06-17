library(readr)
library(dplyr)
library(haven)

# Ensure output directory exists
if (!dir.exists("data/output")) dir.create("data/output", recursive = TRUE, showWarnings = FALSE)

base_path <- "data/input/"

file_names <- c(
  "wave_one_lsype_family_background_2020.tab",
  "wave_two_lsype_family_background_2020.tab",
  "wave_three_lsype_family_background_2020.tab",
  "wave_four_lsype_family_background_2020.tab"
)

read_wf_file <- function(fname) {
  read_delim(file.path(base_path, fname), delim = "\t", col_types = cols())
}

data_list <- lapply(file_names, read_wf_file)
names(data_list) <- c("w1", "w2", "w3", "w4")

# Select relevant variables
w1 <- data_list$w1 %>% select(NSID, W1empsmum, W1empsdad)
w2 <- data_list$w2 %>% select(NSID, W2empsmum, W2empsdad)
w3 <- data_list$w3 %>% select(NSID, W3empsmum, W3empsdad)
w4 <- data_list$w4 %>% select(NSID, w4empsmum, w4empsdad)

# Merge by NSID
merged <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID")

# Harmonise employment codes to standard missing codes
harmonise_employment <- function(x) {
  v <- as.numeric(x)
  v <- ifelse(is.na(v), -3, v)
  v <- ifelse(v == -999.0, -2, v)
  v <- ifelse(v == -99.0, -3, v)
  v <- ifelse(v == -98.0, -3, v)
  v <- ifelse(v == -94.0, -8, v)
  v <- ifelse(v == -92.0, -9, v)
  v <- ifelse(v == -996.0, -2, v)
  v <- ifelse(v == -1.0, -2, v)
  v
}

final_df <- merged %>%
  mutate(
    ecoactma14 = harmonise_employment(W1empsmum),
    ecoactpa14 = harmonise_employment(W1empsdad),
    ecoactma15 = harmonise_employment(W2empsmum),
    ecoactpa15 = harmonise_employment(W2empsdad),
    ecoactma16 = harmonise_employment(W3empsmum),
    ecoactpa16 = harmonise_employment(W3empsdad),
    ecoactma17 = harmonise_employment(w4empsmum),
    ecoactpa17 = harmonise_employment(w4empsdad)
  ) %>%
  select(NSID,
         ecoactma14, ecoactpa14,
         ecoactma15, ecoactpa15,
         ecoactma16, ecoactpa16,
         ecoactma17, ecoactpa17)

write_csv(final_df, "data/output/cleaned_data.csv")
cat("Cleaned data written to data/output/cleaned_data.csv\n")
