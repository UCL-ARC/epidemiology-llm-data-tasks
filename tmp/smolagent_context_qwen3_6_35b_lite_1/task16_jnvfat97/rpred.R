library(dplyr)
library(readr)
library(tidyr)
library(labelled)

# Load all files
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w3 <- read_delim('data/input/wave_three_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Merge all datasets by NSID
df <- full_join(w1, w2, by = 'NSID') %>%
  full_join(w3, by = 'NSID') %>%
  full_join(w4, by = 'NSID')

cat('Merged data dimensions:', nrow(df), 'rows,', ncol(df), 'cols\n')

# ============================================================
# 1. Continuous household income at ages 14 and 15
# ============================================================

# Map missing values for continuous variables (W1GrsswkHH, W2GrsswkHH)
map_missing_continuous <- function(x) {
  x[x == -999] <- -2  # Missing in error
  x[x == -992] <- -2  # No information
  x[x == -99] <- -3   # HH not interviewed
  x[x == -94] <- -8   # Insufficient information
  x[x == -92] <- -9   # Refused
  x[x == -91] <- -1   # Not applicable
  x[x == -3] <- -2    # Not yet paid
  x[x == -1] <- -8    # Don't know
  return(x)
}

# Create continuous income variables
df$inc_w14_cont <- map_missing_continuous(df$W1GrsswkHH)
df$inc_w15_cont <- map_missing_continuous(df$W2GrsswkHH)

# Set variable labels
var_label(df$inc_w14_cont) <- "Household weekly income (continuous) - Age 14"
var_label(df$inc_w15_cont) <- "Household weekly income (continuous) - Age 15"

# ============================================================
# 2. Banded household weekly income at ages 14, 15, 16, 17
# ============================================================

# Map missing values for banded variables
map_missing_banded <- function(x) {
  x[x == -996] <- -2  # No parent in household -> schedule not applicable
  x[x == -99] <- -3   # MP not interviewed -> not interviewed
  x[x == -92] <- -9   # Refused
  x[x == -1] <- -8    # Don't know
  return(x)
}

# Function to create banded income from continuous source
create_banded_from_continuous <- function(x) {
  y <- x
  
  # Map missing codes to standard codes
  y[y == -999] <- -2  # Missing in error
  y[y == -992] <- -2  # No information
  y[y == -99] <- -3   # HH not interviewed
  y[y == -94] <- -8   # Insufficient information
  y[y == -92] <- -9   # Refused
  y[y == -91] <- -1   # Not applicable
  y[y == -3] <- -2    # Not yet paid
  y[y == -1] <- -8    # Don't know
  
  # Initialize result with the missing codes
  band_result <- y
  
  # For valid values, band them using findInterval
  # Use !is.na() to handle NAs properly
  valid_idx <- !is.na(y) & y >= 1
  # findInterval returns 0 for x < 49, 1 for 49 <= x < 99, etc.
  # Adding 1 gives us 1 for x < 49, 2 for 49 <= x < 99, etc.
  band_result[valid_idx] <- findInterval(y[valid_idx], 
                                          c(49, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999),
                                          rightmost.closed = TRUE) + 1
  
  return(band_result)
}

# Create banded income variables
df$inc_w14 <- create_banded_from_continuous(df$W1GrsswkHH)
df$inc_w15 <- create_banded_from_continuous(df$W2GrsswkHH)
df$inc_w16 <- map_missing_banded(df$W3incestw)
df$inc_w17 <- map_missing_banded(df$w4IncEstW)

# Set variable labels for banded variables
var_label(df$inc_w14) <- "Household weekly income (banded) - Age 14"
var_label(df$inc_w15) <- "Household weekly income (banded) - Age 15"
var_label(df$inc_w16) <- "Household weekly income (banded) - Age 16"
var_label(df$inc_w17) <- "Household weekly income (banded) - Age 17"

# ============================================================
# 3. Create final output
# ============================================================

# Keep only NSID and derived variables
output_df <- df %>%
  select(NSID, 
         inc_w14_cont, inc_w15_cont,
         inc_w14, inc_w15, inc_w16, inc_w17)

# Write output
write_csv(output_df, 'data/output/cleaned_data.csv')

cat('Output written successfully\n')
cat('Output dimensions:', nrow(output_df), 'rows,', ncol(output_df), 'cols\n')

# Summary statistics
cat('\nSummary of banded income variables:\n')
for (col in c('inc_w14', 'inc_w15', 'inc_w16', 'inc_w17')) {
  cat(paste0('\n', col, ':\n'))
  print(summary(output_df[[col]]))
}

cat('\nSummary of continuous income variables:\n')
for (col in c('inc_w14_cont', 'inc_w15_cont')) {
  cat(paste0('\n', col, ':\n'))
  print(summary(output_df[[col]]))
}
