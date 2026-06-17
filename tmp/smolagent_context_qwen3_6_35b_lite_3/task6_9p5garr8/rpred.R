library(dplyr)
library(readr)
library(labelled)
library(tidyr)

# Load all files
w1 <- read_tsv("data/input/wave_one_lsype_young_person_2020.tab", show_col_types = FALSE)
w2 <- read_tsv("data/input/wave_two_lsype_family_background_2020.tab", show_col_types = FALSE)
w3 <- read_tsv("data/input/wave_three_lsype_family_background_2020.tab", show_col_types = FALSE)
w4 <- read_tsv("data/input/wave_four_lsype_young_person_2020.tab", show_col_types = FALSE)
w8 <- read_tsv("data/input/ns8_2015_derived.tab", show_col_types = FALSE)
w9d <- read_tsv("data/input/ns9_2022_derived_variables.tab", show_col_types = FALSE)
w9m <- read_tsv("data/input/ns9_2022_main_interview.tab", show_col_types = FALSE)

# Merge all datasets by NSID
df <- w1 %>%
  full_join(w2, by = "NSID") %>%
  full_join(w3, by = "NSID") %>%
  full_join(w4, by = "NSID") %>%
  full_join(w8, by = "NSID") %>%
  full_join(w9d, by = "NSID") %>%
  full_join(w9m, by = "NSID")

# Helper function to convert missing values
clean_region_var <- function(x, default_missing = -3) {
  x <- as.integer(x)
  x[is.na(x)] <- default_missing
  x[x == -94] <- -8
  x[x == -999] <- -2
  x[x == -998] <- -2
  x[x == -997] <- -9
  x[x == -995] <- -2
  x[x == -100] <- -2
  x[x == -97] <- -2
  x[x == -92] <- -9
  x[x == -91] <- -1
  x[x == -99] <- -3
  x
}

df <- df %>%
  mutate(
    # regub15: Urban/Rural at age 15 (from wave2 urbind.x)
    regub15 = clean_region_var(urbind.x, default_missing = -3),
    
    # regub16: Urban/Rural at age 16 (from wave3 urbind.y)
    regub16 = clean_region_var(urbind.y, default_missing = -3),
    
    # regov15: Gov Office Region at age 15 (from wave2 gor.x)
    regov15 = clean_region_var(gor.x, default_missing = -3),
    
    # regov16: Gov Office Region at age 16 (from wave3 gor.y)
    regov16 = clean_region_var(gor.y, default_missing = -3),
    
    # regor25: Gov Office Region at age 25 (from wave8 W8DGOR)
    regor25 = clean_region_var(W8DGOR, default_missing = -3),
    
    # regor32: Gov Office Region at age 32 (from wave9 derived W9DRGN)
    regor32 = clean_region_var(W9DRGN, default_missing = -3),
    
    # regint32: UK/Abroad at age 32 (from wave9 main W9NATIONRES)
    regint32_raw = clean_region_var(W9NATIONRES, default_missing = -3)
  )

# Map regint32: 1-4 (England, Scotland, Wales, NI) -> 1 (In UK), 5 -> 2 (Abroad)
df <- df %>%
  mutate(
    regint32 = case_when(
      regint32_raw %in% c(1, 2, 3, 4) ~ 1L,
      regint32_raw == 5 ~ 2L,
      regint32_raw == -9 ~ -9L,
      regint32_raw == -8 ~ -8L,
      regint32_raw == -3 ~ -3L,
      regint32_raw == -1 ~ -1L,
      TRUE ~ as.integer(regint32_raw)
    )
  )

# Remove the temporary column
df <- df %>% select(-regint32_raw)

# Add labels for categorical variables
# For regub15 and regub16 (Urban/Rural)
urb_labels <- c(
  "Urban >= 10k - sparse" = 1L,
  "Town & Fringe - sparse" = 2L,
  "Village - sparse" = 3L,
  "Hamlet and Isolated Dwelling - sparse" = 4L,
  "Urban >= 10k - less sparse" = 5L,
  "Town & Fringe - less sparse" = 6L,
  "Village - less sparse" = 7L,
  "Hamlet & Isolated Dwelling" = 8L,
  "Don\'t know/insufficient information" = -8L,
  "Not asked at fieldwork stage" = -3L
)
df <- df %>%
  mutate(
    regub15 = labelled::labelled(regub15, labels = urb_labels),
    regub16 = labelled::labelled(regub16, labels = urb_labels)
  )

# For regov15 and regov16 (Gov Office Region - England only)
eng_labels <- c(
  "North East" = 1L,
  "North West" = 2L,
  "Yorkshire and The Humber" = 3L,
  "East Midlands" = 4L,
  "West Midlands" = 5L,
  "East of England" = 6L,
  "London" = 7L,
  "South East" = 8L,
  "South West" = 9L,
  "Don\'t know/insufficient information" = -8L,
  "Not asked at fieldwork stage" = -3L
)
df <- df %>%
  mutate(
    regov15 = labelled::labelled(regov15, labels = eng_labels),
    regov16 = labelled::labelled(regov16, labels = eng_labels)
  )

# For regor25 and regor32 (Gov Office Region - UK-wide)
uks_labels <- c(
  "North East" = 1L,
  "North West" = 2L,
  "Yorkshire and the Humber" = 3L,
  "East Midlands" = 4L,
  "West Midlands" = 5L,
  "East of England" = 6L,
  "London" = 7L,
  "South East" = 8L,
  "South West" = 9L,
  "Wales" = 10L,
  "Scotland" = 11L,
  "Northern Ireland" = 12L,
  "Unknown due to faulty/missing postcode" = 13L,
  "Refused" = -9L,
  "Insufficient information" = -8L,
  "Not applicable" = -1L,
  "Not asked at fieldwork stage" = -3L
)
df <- df %>%
  mutate(
    regor25 = labelled::labelled(regor25, labels = uks_labels),
    regor32 = labelled::labelled(regor32, labels = uks_labels)
  )

# For regint32 (UK/Abroad)
uk_labels <- c(
  "In UK" = 1L,
  "Abroad" = 2L,
  "Refused" = -9L,
  "Don\'t know" = -8L,
  "Not asked at fieldwork stage" = -3L,
  "Not applicable" = -1L
)
df <- df %>%
  mutate(
    regint32 = labelled::labelled(regint32, labels = uk_labels)
  )

# Create final dataframe with only required variables
result <- df %>%
  select(NSID, regub15, regub16, regov15, regov16, regor25, regor32, regint32)

# Write output
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
write_csv(result, "data/output/cleaned_data.csv")

cat("Done. Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(result), "\n")
cat("Variables:", paste(names(result), collapse = ", "), "\n")

# Verify the output
cat("\nVerifying output:\n")
result2 <- read_csv("data/output/cleaned_data.csv", show_col_types = FALSE)
cat("\nFirst 5 rows:\n")
print(head(result2, 5))
cat("\nUnique values in regub15:", sort(unique(result2$regub15)), "\n")
cat("Unique values in regov15:", sort(unique(result2$regov15)), "\n")
cat("Unique values in regor25:", sort(unique(result2$regor25)), "\n")
cat("Unique values in regint32:", sort(unique(result2$regint32)), "\n")