library(readr)
library(dplyr)

# File paths
files <- list(
  wave_one = "data/input/wave_one_lsype_family_background_2020.tab",
  wave_two = "data/input/wave_two_lsype_family_background_2020.tab",
  wave_three = "data/input/wave_three_lsype_family_background_2020.tab",
  wave_four = "data/input/wave_four_lsype_family_background_2020.tab",
  wave_five = "data/input/wave_five_lsype_family_background_2020.tab"
)

# Helper to read only needed columns
read_wave <- function(path, var1, var2){
  cols_spec <- cols(
    NSID = col_character(),
    !!var1 := col_double(),
    !!var2 := col_double()
  )
  df <- read_delim(path, delim = "\t", col_types = cols_spec, na = c(""))
  df %>% distinct(NSID, .keep_all = TRUE)
}

# Read each wave
wf1 <- read_wave(files$wave_one, "W1nsseccatmum", "W1nsseccatdad")
wf2 <- read_wave(files$wave_two, "W2nsseccatmum", "W2nsseccatdad")
wf3 <- read_wave(files$wave_three, "W3cnsseccatmum", "W3cnsseccatdad")
wf4 <- read_wave(files$wave_four, "w4cnsseccatmum", "w4cnsseccatdad")
wf5 <- read_wave(files$wave_five, "w5Cnsseccatmum", "w5Cnsseccatdad")

# Master ID list
master_ids <- bind_rows(wf1, wf2, wf3, wf4, wf5) %>% distinct(NSID)

# Merge all waves
merged <- master_ids %>%
  left_join(wf1, by = "NSID") %>%
  left_join(wf2, by = "NSID") %>%
  left_join(wf3, by = "NSID") %>%
  left_join(wf4, by = "NSID") %>%
  left_join(wf5, by = "NSID")

# Function to collapse fractional to integer and map missing
convert_nssec <- function(x){
  y <- ifelse(is.na(x), NA_real_, floor(x))
  y[y == -98] <- -3
  y
}

# Create target variables
merged <- merged %>%
  mutate(
    nssecma14 = convert_nssec(W1nsseccatmum),
    nssecpa14 = convert_nssec(W1nsseccatdad),
    nssecma15 = convert_nssec(W2nsseccatmum),
    nssecpa15 = convert_nssec(W2nsseccatdad),
    nssecma16 = convert_nssec(W3cnsseccatmum),
    nssecpa16 = convert_nssec(W3cnsseccatdad),
    nssecma17 = convert_nssec(w4cnsseccatmum),
    nssecpa17 = convert_nssec(w4cnsseccatdad),
    nssecma18 = convert_nssec(w5Cnsseccatmum),
    nssecpa18 = convert_nssec(w5Cnsseccatdad)
  )

# Replace remaining NA with -3
merged <- merged %>%
  mutate(across(starts_with("nssec"), ~ replace(., is.na(.), -3)))

# Select final columns
final_df <- merged %>% select(NSID, starts_with("nssec"))

# Write output
write_csv(final_df, "data/output/cleaned_data.csv")

print("Cleaning complete. Output written to data/output/cleaned_data.csv")
