library(haven)
library(dplyr)
library(readr)

# Load all datasets
wave1 <- readr::read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
wave2 <- readr::read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
wave4 <- readr::read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge datasets by NSID
merged_data <- full_join(wave1, wave2, by = 'NSID')
merged_data <- full_join(merged_data, wave4, by = 'NSID')

# Define missing value mapping
map_missing <- function(x) {
  x[is.na(x)] <- -3
  x[x == -999] <- -2
  x[x == -99] <- -3
  x[x == -98] <- -1
  x[x == -94] <- -8
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -1] <- -8
  return(x)
}

# Apply missing value mapping
mother_vars <- c('W1hiqualmum', 'W2hiqualmum', 'w4hiqualmum')
father_vars <- c('W1hiqualdad', 'W2hiqualdad', 'w4hiqualdad')

for (var in mother_vars) {
  merged_data[[var]] <- map_missing(as.numeric(merged_data[[var]]))
}

for (var in father_vars) {
  merged_data[[var]] <- map_missing(as.numeric(merged_data[[var]]))
}

# Consolidate mother's education
educdtlma <- numeric(nrow(merged_data))
for (i in 1:nrow(merged_data)) {
  pos_values <- c(merged_data$W1hiqualmum[i], merged_data$W2hiqualmum[i], merged_data$w4hiqualmum[i])
  pos_values <- pos_values[pos_values > 0]
  if (length(pos_values) > 0) {
    educdtlma[i] <- pos_values[1]
  } else {
    neg_values <- c(merged_data$W1hiqualmum[i], merged_data$W2hiqualmum[i], merged_data$w4hiqualmum[i])
    neg_values <- neg_values[neg_values < 0]
    if (length(neg_values) > 0) {
      educdtlma[i] <- neg_values[1]
    } else {
      educdtlma[i] <- -3
    }
  }
}

# Consolidate father's education
educdtlpa <- numeric(nrow(merged_data))
for (i in 1:nrow(merged_data)) {
  pos_values <- c(merged_data$W1hiqualdad[i], merged_data$W2hiqualdad[i], merged_data$w4hiqualdad[i])
  pos_values <- pos_values[pos_values > 0]
  if (length(pos_values) > 0) {
    educdtlpa[i] <- pos_values[1]
  } else {
    neg_values <- c(merged_data$W1hiqualdad[i], merged_data$W2hiqualdad[i], merged_data$w4hiqualdad[i])
    neg_values <- neg_values[neg_values < 0]
    if (length(neg_values) > 0) {
      educdtlpa[i] <- neg_values[1]
    } else {
      educdtlpa[i] <- -3
    }
  }
}

# Create NVQ mapping function
nvq_map <- function(x) {
  if (x %in% c(1, 2, 3, 4)) return(0)
  if (x %in% c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)) return(1)
  if (x == 18) return(2)
  if (x == 19) return(3)
  if (x == 20) return(4)
  return(NA_real_)
}

# Map to NVQ levels
educma <- sapply(educdtlma, nvq_map)
educpa <- sapply(educdtlpa, nvq_map)

# Create data frame with results
final_data <- data.frame(
  NSID = merged_data$NSID,
  educdtlma = educdtlma,
  educdtlpa = educdtlpa,
  educma = educma,
  educpa = educpa
)

# Write the output
readr::write_csv(final_data, 'data/output/cleaned_data.csv')