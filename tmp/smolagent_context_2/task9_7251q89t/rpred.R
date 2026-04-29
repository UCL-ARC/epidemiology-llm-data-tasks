library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# 1. Load files
w1_path <- 'data/input/wave_one_lsype_family_background_2020.tab'
w2_path <- 'data/input/wave_two_lsype_family_background_2020.tab'
w4_path <- 'data/input/wave_four_lsype_family_background_2020.tab'

w1 <- read_delim(w1_path, delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
w2 <- read_delim(w2_path, delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))
w4 <- read_delim(w4_path, delim = '\t', col_types = cols(.default = 'numeric', NSID = col_character()))

# Merge datasets
df <- w1 %>% 
  full_join(w2, by = 'NSID') %>% 
  full_join(w4, by = 'NSID')

# Mapping function for missing values based on metadata labels
# -9 = Refusal, -8 = DK, -7 = Prefer not to say, -3 = Not asked/NA, -2 = Not applicable/lost, -1 = Item not applicable
map_missing <- function(val, labels) {
  if (is.na(val)) return(-3)
  
  # Create a lookup based on the provided metadata labels
  # We need to identify the label associated with the value
  label <- labels[as.character(val)]
  if (is.na(label)) return(-3)
  
  if (grepl('Refused', label, ignore.case = TRUE)) return(-9)
  if (grepl('Don\'t know', label, ignore.case = TRUE)) return(-8)
  if (grepl('Prefer not to say', label, ignore.case = TRUE)) return(-7)
  if (grepl('not interviewed|not present', label, ignore.case = TRUE)) return(-3)
  if (grepl('lost|information lost|insufficient information', label, ignore.case = TRUE)) return(-2)
  if (grepl('Not applicable', label, ignore.case = TRUE)) return(-1)
  
  return(val)
}

# Extract labels for the specific variables
labels_w1_mum <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable')
labels_w1_dad <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know")
labels_w2_mum <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable')
labels_w2_dad <- c('-999.0' = 'Missing - household data lost', '-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information', '-92.0' = 'Refused', '-91.0' = 'Not applicable', '-1.0' = "Don't know")
labels_w4_mum <- c('-99.0' = 'Mother not interviewed', '-98.0' = 'Mother not present', '-94.0' = 'Insufficient information')
labels_w4_dad <- c('-99.0' = 'Father not interviewed', '-98.0' = 'Father not present', '-94.0' = 'Insufficient information')

# Helper to process columns
process_var <- function(vec, labels) {
  sapply(vec, function(x) map_missing(x, labels))
}

df <- df %>%
  mutate(
    m1 = process_var(W1hiqualmum, labels_w1_mum),
    d1 = process_var(W1hiqualdad, labels_w1_dad),
    m2 = process_var(W2hiqualmum, labels_w2_mum),
    d2 = process_var(W2hiqualdad, labels_w2_dad),
    m4 = process_var(w4hiqualmum, labels_w4_mum),
    d4 = process_var(w4hiqualdad, labels_w4_dad)
  )

# Consolidation Logic: First positive (1-20), then first negative, then -3
consolidate <- function(v1, v2, v4) {
  res <- numeric(length(v1))
  for (i in seq_along(v1)) {
    vals <- c(v1[i], v2[i], v4[i])
    # 1. First positive value
    pos <- vals[vals > 0 & !is.na(vals)]
    if (length(pos) > 0) {
      res[i] <- pos[1]
    } else {
      # 2. First negative code
      neg <- vals[vals < 0 & !is.na(vals)]
      if (length(neg) > 0) {
        res[i] <- neg[1]
      } else {
        res[i] <- -3
      }
    }
  }
  return(res)
}

df <- df %>%
  mutate(
    mum_detailed = consolidate(m1, m2, m4),
    dad_detailed = consolidate(d1, d2, d4)
  )

# Collapsed mapping
collapse_edu <- function(x) {
  case_when(
    x >= 1 & x <= 4 ~ 0, # Higher Degree, First Degree, HE Diploma, HNC/HND/NVQ4
    x >= 5 & x <= 17 ~ 1, # Teaching/Nursing, A Levels, OND/ONC, NVQ3, CSYS, Scottish Higher, AS Level, Trade app, NVQ2, GCSE A-C, GCSE D-E, NVQ1
    x == 18 ~ 2,         # Youth training
    x == 19 ~ 3,         # Unspecified
    x == 20 ~ 4,         # No qualification
    TRUE ~ x             # Missing codes pass through
  )
}

df <- df %>%
  mutate(
    mum_collapsed = collapse_edu(mum_detailed),
    dad_collapsed = collapse_edu(dad_detailed)
  )

# Final Selection
final_df <- df %>%
  select(NSID, mum_collapsed, mum_detailed, dad_collapsed, dad_detailed)

# Labels
# Detailed: 1-20
# Collapsed: 0-4
# Missing: -1 to -9

write_csv(final_df, 'data/output/cleaned_data.csv')
