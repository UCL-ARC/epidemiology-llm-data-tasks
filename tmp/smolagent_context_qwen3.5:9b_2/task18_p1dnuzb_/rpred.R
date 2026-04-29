library(haven)
library(dplyr)
library(purrr)
library(labelled)

data_dir <- "data/input"
output_dir <- "data/output"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

load_tab_file <- function(filepath) {
  readr::read_delim(filepath, delim = "\t")
}

wave1 <- load_tab_file(file.path(data_dir, "wave_one_lsype_young_person_2020.tab"))
wave2 <- load_tab_file(file.path(data_dir, "wave_two_lsype_young_person_2020.tab"))
wave4 <- load_tab_file(file.path(data_dir, "wave_four_lsype_young_person_2020.tab"))
ns8_self <- load_tab_file(file.path(data_dir, "ns8_2015_self_completion.tab"))
ns8_derived <- load_tab_file(file.path(data_dir, "ns8_2015_derived.tab"))
ns9_main <- load_tab_file(file.path(data_dir, "ns9_2022_main_interview.tab"))
ns9_derived <- load_tab_file(file.path(data_dir, "ns9_2022_derived_variables.tab"))

ghq12_items_wave2 <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", 
                       "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", 
                       "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")

ghq12_items_wave4 <- c("W4ConcenYP", "W4NoSleepYP", "W4UsefulYP", "W4DecideYP", 
                       "W4StrainYP", "W4DifficYP", "W4ActivYP", "W4ProbsYP", 
                       "W4DepressYP", "W4NoConfYP", "W4WthlessYP", "W4HappyYP")

ghq12_items_wave8 <- c("W8GHQ12_1", "W8GHQ12_2", "W8GHQ12_3", "W8GHQ12_4", 
                       "W8GHQ12_5", "W8GHQ12_6", "W8GHQ12_7", "W8GHQ12_8", 
                       "W8GHQ12_9", "W8GHQ12_10", "W8GHQ12_11", "W8GHQ12_12")

ghq12_items_wave9 <- c("W9GHQ12_1", "W9GHQ12_2", "W9GHQ12_3", "W9GHQ12_4", 
                       "W9GHQ12_5", "W9GHQ12_6", "W9GHQ12_7", "W9GHQ12_8", 
                       "W9GHQ12_9", "W9GHQ12_10", "W9GHQ12_11", "W9GHQ12_12")

wave2_sel <- wave2 %>% select(NSID, all_of(ghq12_items_wave2), W2ghq12scr)
wave4_sel <- wave4 %>% select(NSID, all_of(ghq12_items_wave4), W4ghq12scr)
ns8_sel <- ns8_self %>% select(NSID, all_of(ghq12_items_wave8))
ns9_sel <- ns9_main %>% select(NSID, all_of(ghq12_items_wave9))

harmonize_early <- function(x) {
  x <- as.numeric(x)
  x[x == -96] <- -3
  x[x == -99] <- -3
  x[x == -97] <- -9
  x[x == -92] <- -9
  x[is.na(x)] <- -3
  return(x)
}

harmonize_later <- function(x) {
  x <- as.numeric(x)
  x[is.na(x)] <- -3
  return(x)
}

wave2_sel <- wave2_sel %>% mutate(ghq15 = harmonize_early(W2ghq12scr))
wave4_sel <- wave4_sel %>% mutate(ghq17 = harmonize_early(W4ghq12scr))

compute_ghqtl <- function(data, item_vars, new_var) {
  n <- nrow(data)
  result <- rep(NA_integer_, n)
  
  for (i in 1:n) {
    vals <- as.numeric(data[i, item_vars])
    all_na <- all(is.na(vals))
    has_neg <- any(vals < 0)
    
    if (all_na) {
      result[i] <- -3
    } else if (has_neg) {
      result[i] <- -8
    } else {
      result[i] <- sum(vals, na.rm = TRUE)
    }
  }
  
  data[[new_var]] <- result
  return(data)
}

wave2_sel <- compute_ghqtl(wave2_sel, ghq12_items_wave2, "ghqtl15")
wave4_sel <- compute_ghqtl(wave4_sel, ghq12_items_wave4, "ghqtl17")
ns8_sel <- compute_ghqtl(ns8_sel, ghq12_items_wave8, "ghqtl25")
ns9_sel <- compute_ghqtl(ns9_sel, ghq12_items_wave9, "ghqtl32")

final_data <- wave2_sel %>%
  full_join(wave4_sel, by = "NSID") %>%
  full_join(ns8_sel, by = "NSID") %>%
  full_join(ns9_sel, by = "NSID")

ns8_derived_sel <- ns8_derived %>% select(NSID, W8DGHQSC) %>% rename(ghq25 = W8DGHQSC)
ns8_derived_sel <- ns8_derived_sel %>% mutate(ghq25 = harmonize_later(ghq25))
final_data <- final_data %>% full_join(ns8_derived_sel, by = "NSID")

ns9_derived_sel <- ns9_derived %>% select(NSID, W9DGHQSC) %>% rename(ghq32 = W9DGHQSC)
ns9_derived_sel <- ns9_derived_sel %>% mutate(ghq32 = harmonize_later(ghq32))
final_data <- final_data %>% full_join(ns9_derived_sel, by = "NSID")

# Create labels as a named vector with character names
label_names <- c("-1" = "Item not applicable", 
                 "-2" = "Script error/information lost", 
                 "-3" = "Not asked at the fieldwork stage/participated/interviewed", 
                 "-8" = "Don't know/insufficient information", 
                 "-9" = "Refusal")

ghq_vars <- c("ghq15", "ghq17", "ghq25", "ghq32", "ghqtl15", "ghqtl17", "ghqtl25", "ghqtl32")

for (var in ghq_vars) {
  final_data[[var]] <- haven::labelled(as.character(final_data[[var]]), labels = label_names)
}

final_data <- final_data %>% mutate(across(all_of(ghq_vars), as.factor))

final_data <- final_data %>% select(NSID, all_of(ghq_vars))

write.csv(final_data, file.path(output_dir, "cleaned_data.csv"), row.names = FALSE)

print("Script completed successfully.")
print(paste("Total rows:", nrow(final_data)))
print(paste("Total columns:", ncol(final_data)))