library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

# Define missing value labels
# To use labelled() from haven/labelled, we can provide labels as a named vector
# where names are the labels and the values are the codes, OR use a specific format.
# However, the error "Can't convert labels <character> to match type of x <double>"
# indicates that haven::labelled expects the 'labels' argument to be a named vector
# where the values in the vector match the type of the data (double).

missing_labels_vec <- c(
  "Item not applicable" = -1,
  "Script error/information lost" = -2,
  "Not asked at the fieldwork stage/participated/interviewed" = -3,
  "Don't know/insufficient information" = -8,
  "Refusal" = -9
)

# 1. File Loading
file2 <- "data/input/wave_two_lsype_young_person_2020.tab"
vars2 <- c("NSID", "W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP", "W2ghq12scr")
data2 <- read_delim(file2, delim = "\t", col_select = all_of(vars2))

file4 <- "data/input/wave_four_lsype_young_person_2020.tab"
vars4 <- c("NSID", "W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP", "W4ghq12scr")
data4 <- read_delim(file4, delim = "\t", col_select = all_of(vars4))

file8_main <- "data/input/ns8_2015_self_completion.tab"
vars8_main <- c("NSID", paste0("W8GHQ12_", 1:12))
data8_main <- read_delim(file8_main, delim = "\t", col_select = all_of(vars8_main))

file8_der <- "data/input/ns8_2015_derived.tab"
vars8_der <- c("NSID", "W8DGHQSC")
data8_der <- read_delim(file8_der, delim = "\t", col_select = all_of(vars8_der))

file9_main <- "data/input/ns9_2022_main_interview.tab"
vars9_main <- c("NSID", paste0("W9GHQ12_", 1:12))
data9_main <- read_delim(file9_main, delim = "\t", col_select = all_of(vars9_main))

file9_der <- "data/input/ns9_2022_derived_variables.tab"
vars9_der <- c("NSID", "W9DGHQSC")
data9_der <- read_delim(file9_der, delim = "\t", col_select = all_of(vars9_der))

merged_data <- data2 %>%
  full_join(data4, by = "NSID") %>%
  full_join(data8_main, by = "NSID") %>%
  full_join(data8_der, by = "NSID") %>%
  full_join(data9_main, by = "NSID") %>%
  full_join(data9_der, by = "NSID")

compute_ghqtl <- function(data, items) {
  mat <- data[, items]
  res <- apply(mat, 1, function(x) {
    if (all(is.na(x))) return(-3)
    if (any(x < 0, na.rm = TRUE)) return(-8)
    sum(as.numeric(x), na.rm = TRUE)
  })
  return(res)
}

merged_data$ghqtl15 <- compute_ghqtl(merged_data, c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP"))
merged_data$ghqtl17 <- compute_ghqtl(merged_data, c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP"))
merged_data$ghqtl25 <- compute_ghqtl(merged_data, paste0("W8GHQ12_", 1:12))
merged_data$ghqtl32 <- compute_ghqtl(merged_data, paste0("W9GHQ12_", 1:12))

harmonise_early <- function(x) {
  x <- as.numeric(x)
  x[x == -96 | x == -99] <- -3
  x[x == -97 | x == -92] <- -9
  x[is.na(x)] <- -3
  return(x)
}

harmonise_late <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  return(x)
}

merged_data$ghq15 <- harmonise_early(merged_data$W2ghq12scr)
merged_data$ghq17 <- harmonise_early(merged_data$W4ghq12scr)
merged_data$ghq25 <- harmonise_late(merged_data$W8DGHQSC)
merged_data$ghq32 <- harmonise_late(merged_data$W9DGHQSC)

final_vars <- c("NSID", "ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")
final_df <- merged_data[, final_vars]

derived_cols <- setdiff(final_vars, "NSID")
for (col in derived_cols) {
  # Ensure numeric before applying labels
  val_numeric <- as.numeric(final_df[[col]])
  # labelled() expects labels = c("Label1" = value1, "Label2" = value2)
  final_df[[col]] <- labelled(val_numeric, labels = missing_labels_vec)
  final_df[[col]] <- as.factor(final_df[[col]])
}

write_csv(final_df, "data/output/cleaned_data.csv")