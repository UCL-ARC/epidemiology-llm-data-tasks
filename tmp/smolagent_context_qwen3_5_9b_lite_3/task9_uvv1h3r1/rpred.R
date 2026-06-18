library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(labelled)
library(readr)

# Load all datasets
w1 <- read_delim('data/input/wave_one_lsype_family_background_2020.tab', delim = '\t')
w2 <- read_delim('data/input/wave_two_lsype_family_background_2020.tab', delim = '\t')
w4 <- read_delim('data/input/wave_four_lsype_family_background_2020.tab', delim = '\t')

# Merge all datasets
merged <- full_join(w1, w2, by = 'NSID')
merged <- full_join(merged, w4, by = 'NSID')

# Define NVQ level mapping function (vectorized)
map_nvq_level <- function(x) {
  result <- integer(length(x))
  
  # Missing value codes - return as-is (these are already in the data)
  missing_codes <- c(-1, -2, -3, -8, -9, -94, -98, -99, -999)
  
  for (i in seq_along(x)) {
    curr <- x[i]
    
    if (curr %in% missing_codes) {
      result[i] <- curr
    } else {
      # Map detailed qualifications (1-20) to 5 NVQ levels
      # Level 5 (highest): 1=Higher Degree, 2=First Degree, 3=HE Diploma, 4=HNC/HND/NVQ4, 5=Teaching qual non-degree, 6=Nursing qual non-degree
      if (curr %in% c(1, 2, 3, 4, 5, 6)) {
        result[i] <- 5L
      } else if (curr %in% c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16)) {
        result[i] <- 3L
      } else if (curr %in% c(17, 18, 19, 20)) {
        result[i] <- 1L
      } else {
        result[i] <- 1L  # Default to lowest
      }
    }
  }
  return(result)
}

# Create detailed variable (20 categories) - keep earliest valid
merged <- merged %>%
  mutate(
    educdtlma = case_when(
      !is.na(W1hiqualmum) & W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ as.integer(W1hiqualmum),
      !is.na(W2hiqualmum) & W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ as.integer(W2hiqualmum),
      !is.na(w4hiqualmum) & w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ as.integer(w4hiqualmum),
      TRUE ~ NA_integer_
    ),
    educdtlpa = case_when(
      !is.na(W1hiqualdad) & W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ as.integer(W1hiqualdad),
      !is.na(W2hiqualdad) & W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ as.integer(W2hiqualdad),
      !is.na(w4hiqualdad) & w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ as.integer(w4hiqualdad),
      TRUE ~ NA_integer_
    )
  )

# Create 5-level NVQ collapsed variable for mother
merged <- merged %>%
  mutate(
    educma = case_when(
      !is.na(W1hiqualmum) & W1hiqualmum >= 1 & W1hiqualmum <= 20 ~ as.integer(map_nvq_level(W1hiqualmum)),
      !is.na(W2hiqualmum) & W2hiqualmum >= 1 & W2hiqualmum <= 20 ~ as.integer(map_nvq_level(W2hiqualmum)),
      !is.na(w4hiqualmum) & w4hiqualmum >= 1 & w4hiqualmum <= 20 ~ as.integer(map_nvq_level(w4hiqualmum)),
      TRUE ~ NA_integer_
    )
  )

# Create 5-level NVQ collapsed variable for father
merged <- merged %>%
  mutate(
    educpa = case_when(
      !is.na(W1hiqualdad) & W1hiqualdad >= 1 & W1hiqualdad <= 20 ~ as.integer(map_nvq_level(W1hiqualdad)),
      !is.na(W2hiqualdad) & W2hiqualdad >= 1 & W2hiqualdad <= 20 ~ as.integer(map_nvq_level(W2hiqualdad)),
      !is.na(w4hiqualdad) & w4hiqualdad >= 1 & w4hiqualdad <= 20 ~ as.integer(map_nvq_level(w4hiqualdad)),
      TRUE ~ NA_integer_
    )
  )

# Remove source variables but keep only the derived ones
output_vars <- c('NSID', 'educdtlma', 'educdtlpa', 'educma', 'educpa')
final_df <- merged %>% select(all_of(output_vars))

write_csv(final_df, 'data/output/cleaned_data.csv')
cat('Script completed successfully\n')
