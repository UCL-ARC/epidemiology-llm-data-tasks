library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define standard missing codes
# -9 = Refusal
# -8 = Don't know/insufficient information
# -1 = Item not applicable
# -3 = Not asked/interviewed
# -2 = Script error/lost

# Helper function to harmonize missing values based on a map
# Map: source_code -> standard_code
harmonize_missing <- function(x, map) {
  x <- as.numeric(x)
  # First, handle explicit map
  for (src in names(map)) {
    val <- as.numeric(src)
    x[x == val] <- map[[src]]
  }
  # Map NAs to -3
  x[is.na(x)] <- -3
  return(x)
}

# Load datasets
file_14 <- read_delim("data/input/wave_one_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
file_15 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
file_16 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))
file_17 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(.default = "c"))

# --- Wave 14 (Continuous) ---
map14 <- c("-92" = -9, "-94" = -8, "-1" = -8, "-91" = -1, "-3" = -1, "-99" = -3, "-992" = -3, "-999" = -2)
incwhhcnt14 <- harmonize_missing(file_14$W1GrsswkHH, map14)

# Banding for Age 14/15: 
# 1: <50, 2: 50-99, 3: 100-199, 4: 200-299, 5: 300-399, 6: 400-499, 7: 500-599, 8: 600-699, 9: 700-799, 10: 800-899, 11: 900-999, 12: 1000+
incwhh14 <- rep(NA, length(incwhhcnt14))
valid_idx <- incwhhcnt14 >= 0
vals <- incwhhcnt14[valid_idx]
incwhh14[valid_idx] <- cut(vals, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf), labels = FALSE, right = TRUE)
incwhh14[!valid_idx] <- incwhhcnt14[!valid_idx]

# --- Wave 15 (Continuous) ---
map15 <- c("-92" = -9, "-94" = -8, "-1" = -8, "-91" = -1, "-3" = -1, "-99" = -3, "-992" = -3, "-999" = -2)
incwhhcnt15 <- harmonize_missing(file_15$W2GrsswkHH, map15)

incwhh15 <- rep(NA, length(incwhhcnt15))
valid_idx <- incwhhcnt15 >= 0
vals <- incwhhcnt15[valid_idx]
incwhh15[valid_idx] <- cut(vals, breaks = c(-Inf, 49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, Inf), labels = FALSE, right = TRUE)
incwhh15[!valid_idx] <- incwhhcnt15[!valid_idx]

# --- Wave 16 (Categorical) ---
map16 <- c("-92" = -9, "-1" = -8, "-99" = -3)
incwhh16_raw <- harmonize_missing(file_16$W3incestw, map16)

# --- Wave 17 (Categorical) ---
map17 <- c("-92" = -9, "-1" = -8, "-996" = -3, "-99" = -3)
incwhh17_raw <- harmonize_missing(file_17$w4IncEstW, map17)

# Final Dataset Construction
df14 <- data.frame(NSID = file_14$NSID, incwhhcnt14 = incwhhcnt14, incwhh14 = incwhh14)
df15 <- data.frame(NSID = file_15$NSID, incwhhcnt15 = incwhhcnt15, incwhh15 = incwhh15)
df16 <- data.frame(NSID = file_16$NSID, incwhh16 = incwhh16_raw)
df17 <- data.frame(NSID = file_17$NSID, incwhh17 = incwhh17_raw)

final_df <- df14 %>%
  full_join(df15, by = "NSID") %>%
  full_join(df16, by = "NSID") %>%
  full_join(df17, by = "NSID")

# Apply standard NA -> -3
final_df[is.na(final_df)] <- -3

# Factor conversion for banded variables
band_labels <- c("-9" = "Refusal", "-8" = "Don't know/insufficient", "-3" = "Not asked", "-2" = "Script error", "-1" = "Not applicable")
# Note: The levels 1-12 are income categories. We combine them with the missing labels.

create_banded_factor <- function(x) {
  # Create a character vector for labels
  # For values 1-12, we keep them as is or can label them. 
  # The prompt asks for labels for all levels.
  levels_map <- c("1"="Up to £49", "2"="£50-£99", "3"="£100-£199", "4"="£200-£299", "5"="£300-£399", "6"="£400-£499", "7"="£500-£599", "8"="£600-£699", "9"="£700-£799", "10"="£800-£899", "11"="£900-£999", "12"="£1,000 or more")
  # Merge with missing labels
  full_map <- c(band_labels, levels_map)
  
  # Map values to labels
  res <- as.character(x)
  names(full_map) <- as.character(as.numeric(names(full_map)))
  
  # Apply labels
  out <- sapply(res, function(v) {
    if(v %in% names(full_map)) return(full_map[[v]]) else return(v)
  })
  return(factor(out, levels = full_map))
}

# Applying factor labels to banded variables
final_df$incwhh14 <- create_banded_factor(final_df$incwhh14)
final_df$incwhh15 <- create_banded_factor(final_df$incwhh15)
final_df$incwhh16 <- create_banded_factor(final_df$incwhh16)
final_df$incwhh17 <- create_banded_factor(final_df$incwhh17)

# Select required variables
final_df <- final_df %>%
  select(NSID, incwhh14, incwhh15, incwhhcnt14, incwhhcnt15, incwhh16, incwhh17)

# Save to CSV
write_csv(final_df, "data/output/cleaned_data.csv")
