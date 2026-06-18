library(dplyr)
library(readr)
library(labelled)
library(tidyr)

# Load the three wave files
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t', show_col_types = FALSE)

# Function to harmonize missing codes to standard scheme
harmonize_missing <- function(x) {
  x <- as.numeric(x)
  # Map specific codes to standard scheme
  x[x == -999] <- -2   # Missing - household data lost
  x[x == -99] <- -3    # Not interviewed
  x[x == -98] <- -3    # Not present
  x[x == -94] <- -8    # Insufficient information
  x[x == -92] <- -9    # Refused
  x[x == -91] <- -1    # Not applicable
  x[x == -1] <- -8     # Don't know
  # Convert any remaining NA to -3 (not asked)
  x[is.na(x)] <- -3
  return(x)
}

# Function to consolidate: scan waves in order, take first positive (1-20),
# if none then first negative code, if none then -3
consolidate_educ <- function(w1var, w2var, w4var) {
  w1var <- harmonize_missing(w1var)
  w2var <- harmonize_missing(w2var)
  w4var <- harmonize_missing(w4var)
  
  result <- rep(-3, length(w1var))  # default if no value exists at all
  
  for (i in seq_along(w1var)) {
    vals <- c(w1var[i], w2var[i], w4var[i])
    has_value <- !is.na(vals) & vals != -3
    if (!any(has_value)) {
      result[i] <- -3
    } else {
      # Get the first valid value (non-NA, non -3)
      first_valid <- vals[has_value][1]
      result[i] <- first_valid
    }
  }
  return(result)
}

# Merge all three files by NSID
df <- full_join(w1, w2, by = 'NSID')
df <- full_join(df, w4, by = 'NSID')

# Derive detailed consolidated variables (20-category scheme)
df$educdtlma <- consolidate_educ(df$W1hiqualmum, df$W2hiqualmum, df$w4hiqualmum)
df$educdtlpa <- consolidate_educ(df$W1hiqualdad, df$W2hiqualdad, df$w4hiqualdad)

# Derive collapsed 5-level NVQ scheme from detailed variables
collapse_nvq <- function(educ_dtl) {
  result <- educ_dtl  # start as copy
  # Map detailed codes to collapsed codes
  # 0 = NVQ 4-5: codes 1,2,3,4
  result[educ_dtl %in% c(1, 2, 3, 4)] <- 0
  # 1 = NVQ 1-3: codes 5-17
  result[educ_dtl %in% c(5:17)] <- 1
  # 2 = Youth training / skill seekers: code 18
  result[educ_dtl == 18] <- 2
  # 3 = Qualification level unspecified: code 19
  result[educ_dtl == 19] <- 3
  # 4 = No qualification mentioned: code 20
  result[educ_dtl == 20] <- 4
  # Keep missing codes as-is (they are already in result since we copied)
  return(result)
}

df$educma <- collapse_nvq(df$educdtlma)
df$educpa <- collapse_nvq(df$educdtlpa)

# Select only NSID and final derived variables
out <- df %>% select(NSID, educdtlma, educdtlpa, educma, educpa)

# Add value labels using haven::labelled with named numeric vector
# Create labels as named numeric vector
dtl_labels <- c(`1` = 1, `2` = 2, `3` = 3, `4` = 4, `5` = 5, `6` = 6, `7` = 7, `8` = 8, `9` = 9, `10` = 10, `11` = 11, `12` = 12, `13` = 13, `14` = 14, `15` = 15, `16` = 16, `17` = 17, `18` = 18, `19` = 19, `20` = 20)
names(dtl_labels) <- c('Higher Degree', 'First Degree', 'HE Diploma', 'HNC/HND/NVQ4', 'Teaching qualification, non-degree', 'Nursing qualification, non-degree', 'A Levels', 'OND/ONC', 'City and guilds part III, NVQ3', 'CSYS', 'Scottish Higher Grade', 'AS Level', 'Trade apprenticeship', 'City and guilds part II, NVQ2', 'GCSE grade A-C and equivalent', 'GCSE grade D-E and equivalent', 'City and guilds part I, NVQ1', 'Youth training, skill seekers', 'Qualification, level unspecified', 'No qualification mentioned')

out$educdtlma <- haven::labelled(out$educdtlma, labels = dtl_labels)
out$educdtlpa <- haven::labelled(out$educdtlpa, labels = dtl_labels)

collapsed_labels <- c(`0` = 0, `1` = 1, `2` = 2, `3` = 3, `4` = 4)
names(collapsed_labels) <- c('NVQ 4-5: Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4', 'NVQ 1-3: teaching/nursing through City & Guilds Part I / NVQ1', 'Youth training / skill seekers', 'Qualification, level unspecified', 'No qualification mentioned')

out$educma <- haven::labelled(out$educma, labels = collapsed_labels)
out$educpa <- haven::labelled(out$educpa, labels = collapsed_labels)

# Write output
write_csv(out, 'data/output/cleaned_data.csv')

cat('Output written successfully\n')
cat('Dimensions:', dim(out), '\n')
cat('Columns:', names(out), '\n')
