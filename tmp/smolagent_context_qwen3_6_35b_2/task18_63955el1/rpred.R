library(dplyr)
library(readr)
library(tidyr)

# Ensure output directory exists
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

# --- Helper function for item-summed Likert scores ---
# GHQ items are 1-4 where 1,2 = no problem (score 0), 3,4 = problem (score 1)
compute_likert_sum <- function(items) {
  has_negative <- any(items < 0, na.rm = TRUE)
  all_na <- all(is.na(items))
  
  if (all_na) {
    return(-3)
  }
  if (has_negative) {
    return(-8)
  }
  # Recode: 1→0, 2→0, 3→1, 4→1, then sum (valid range 0-12)
  recoded <- ifelse(items >= 3, 1, 0)
  return(sum(recoded))
}

# --- Helper function for pre-derived caseness scores ---
map_prederived_w2w4 <- function(x) {
  result <- x
  result[result == -999 | result == -998 | result == -997 | result == -995] <- -2
  result[result == -99] <- -3
  result[result == -97] <- -9
  result[result == -96] <- -7
  result[result == -92] <- -9
  result[result == -91] <- -1
  result[result == -1] <- -8
  result[is.na(result)] <- -3
  return(result)
}

map_prederived_w8w9 <- function(x) {
  result <- x
  result[result == -9] <- -9
  result[result == -8] <- -8
  result[result == -3] <- -3
  result[result == -1] <- -1
  result[is.na(result)] <- -3
  return(result)
}

# --- Load Wave 2 (Age 15) ---
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Use column indices 460:471 for W2 GHQ items
w2_ghq_items <- w2[, 460:471]

# Compute item-summed Likert scores for W2
w2$ghqtl15 <- apply(w2_ghq_items, 1, function(row) compute_likert_sum(as.numeric(row)))

w2$ghq15 <- map_prederived_w2w4(w2$W2ghq12scr)
w2 <- w2 %>% select(NSID, ghq15, ghqtl15)

# --- Load Wave 4 (Age 17) ---
w4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)

# Use column indices 805:816 for W4 GHQ items
w4_ghq_items <- w4[, 805:816]

# Compute item-summed Likert scores for W4
w4$ghqtl17 <- apply(w4_ghq_items, 1, function(row) compute_likert_sum(as.numeric(row)))

w4$ghq17 <- map_prederived_w2w4(w4$W4ghq12scr)
w4 <- w4 %>% select(NSID, ghq17, ghqtl17)

# --- Load Wave 8 (Age 25) ---
w8_items <- read_delim("data/input/ns8_2015_self_completion.tab", delim = "\t", show_col_types = FALSE)
w8_derived <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)

# Use column indices 29:40 for W8 GHQ items
w8_ghq_items <- w8_items[, 29:40]

# Compute item-summed Likert scores for W8
w8_items$ghqtl25 <- apply(w8_ghq_items, 1, function(row) compute_likert_sum(as.numeric(row)))

w8_derived$ghq25 <- map_prederived_w8w9(w8_derived$W8DGHQSC)
w8_derived <- w8_derived %>% select(NSID, ghq25)

# --- Load Wave 9 (Age 32) ---
w9_items <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)
w9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)

# Use column indices 1557:1568 for W9 GHQ items
w9_ghq_items <- w9_items[, 1557:1568]

# Compute item-summed Likert scores for W9
w9_items$ghqtl32 <- apply(w9_ghq_items, 1, function(row) compute_likert_sum(as.numeric(row)))

w9_derived$ghq32 <- map_prederived_w8w9(w9_derived$W9DGHQSC)
w9_derived <- w9_derived %>% select(NSID, ghq32)

# --- Merge all datasets ---
result <- full_join(w2, w4, by = "NSID")
result <- full_join(result, w8_items[, c("NSID", "ghqtl25")], by = "NSID")
result <- full_join(result, w8_derived, by = "NSID")
result <- full_join(result, w9_items[, c("NSID", "ghqtl32")], by = "NSID")
result <- full_join(result, w9_derived, by = "NSID")

# --- Select final variables ---
final <- result %>% select(NSID, ghqtl15, ghqtl17, ghqtl25, ghqtl32, ghq15, ghq17, ghq25, ghq32)

# --- Write output ---
write_csv(final, "data/output/cleaned_data.csv")

cat("Done! Output written to data/output/cleaned_data.csv\n")
cat("Number of rows:", nrow(final), "\n")
cat("Columns:", paste(names(final), collapse = ", "), "\n")

# --- Quick validation ---
cat("\nItem-summed summary:\n")
print(summary(final[, c("ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")]))
cat("\nPre-derived summary:\n")
print(summary(final[, c("ghq15", "ghq17", "ghq25", "ghq32")]))
