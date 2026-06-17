library(readr)
library(dplyr)
library(labelled)

# 1. Load the five waves
wave1 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
wave5 <- read_delim("data/input/wave_five_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)

# 2. Merge waves by NSID
merged <- wave1 %>%
  full_join(wave2, by = "NSID") %>%
  full_join(wave3, by = "NSID") %>%
  full_join(wave4, by = "NSID") %>%
  full_join(wave5, by = "NSID")

# 3. Function to collapse NS‑SEC fractional codes
collapse_nssec <- function(x){
  res <- x
  # Map specific negative codes
  res[res == -98]  <- -3   # Parent not present
  res[res == -999] <- -2   # Household data lost
  res[res == -99]  <- -3   # Parent not interviewed
  res[res == -94]  <- -9   # Insufficient information
  # Truncate fractional part to major category
  pos_mask <- !is.na(res) & res >= 0
  res[pos_mask] <- as.integer(res[pos_mask])
  # Replace remaining NA with the standard missing code
  res[is.na(res)] <- -3
  return(res)
}

# 4. Derive the ten NS‑SEC variables
cleaned <- merged %>%
  mutate(
    nssecma14 = collapse_nssec(as.numeric(W1nsseccatmum)),
    nssecpa14 = collapse_nssec(as.numeric(W1nsseccatdad)),
    nssecma15 = collapse_nssec(as.numeric(W2nsseccatmum)),
    nssecpa15 = collapse_nssec(as.numeric(W2nsseccatdad)),
    nssecma16 = collapse_nssec(as.numeric(W3cnsseccatmum)),
    nssecpa16 = collapse_nssec(as.numeric(W3cnsseccatdad)),
    nssecma17 = collapse_nssec(as.numeric(w4cnsseccatmum)),
    nssecpa17 = collapse_nssec(as.numeric(w4cnsseccatdad)),
    nssecma18 = collapse_nssec(as.numeric(w5Cnsseccatmum)),
    nssecpa18 = collapse_nssec(as.numeric(w5Cnsseccatdad))
  ) %>%
  select(NSID, nssecma14, nssecpa14, nssecma15, nssecpa15, nssecma16, nssecpa16, nssecma17, nssecpa17, nssecma18, nssecpa18)

# 5. Write cleaned data
write_csv(cleaned, "data/output/cleaned_data.csv")
