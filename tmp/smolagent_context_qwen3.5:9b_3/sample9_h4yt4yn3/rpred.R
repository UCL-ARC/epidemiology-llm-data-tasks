library(haven)
library(dplyr)
library(purrr)
library(labelled)
library(readr)

setwd("/home/jovyan/epidemiology-llm-data-tasks/tmp/smolagent_context/sample9_h4yt4yn3")

input_dir <- "data/input/"
output_file <- "data/output/cleaned_data.csv"

if (!dir.exists(dirname(output_file))) {
  dir.create(dirname(output_file), recursive = TRUE)
}

wave1_meta <- list(file = "wave_one_lsype_family_background_2020.tab", mum_var = "W1hiqualmum", dad_var = "W1hiqualdad")
wave2_meta <- list(file = "wave_two_lsype_family_background_2020.tab", mum_var = "W2hiqualmum", dad_var = "W2hiqualdad")
wave4_meta <- list(file = "wave_four_lsype_family_background_2020.tab", mum_var = "w4hiqualmum", dad_var = "w4hiqualdad")

all_waves <- list(wave1_meta, wave2_meta, wave4_meta)

load_wave <- function(meta) {
  data <- read_delim(file.path(input_dir, meta$file), delim = "\t", col_types = cols())
  return(data)
}

data_list <- lapply(all_waves, load_wave)
names(data_list) <- sapply(all_waves, function(w) w$file)

cat("Loaded data from\n")
for (i in 1:3) {
  cat("  ", names(data_list)[i], ": ", nrow(data_list[[i]]), " rows\n", sep="")
}

final_data <- data_list[[1]]
cat("\nMerging waves by NSID...\n")

for (i in 2:3) {
  final_data <- full_join(final_data, data_list[[i]], by = "NSID")
}

cat("Merged dataset has ", nrow(final_data), " rows\n")

cat("\n--- Consolidation ---\n")

mum_vars <- c("W1hiqualmum", "W2hiqualmum", "w4hiqualmum")
dad_vars <- c("W1hiqualdad", "W2hiqualdad", "w4hiqualdad")

mum_w1 <- final_data[[mum_vars[1]]]
mum_w2 <- final_data[[mum_vars[2]]]
mum_w4 <- final_data[[mum_vars[3]]]

dad_w1 <- final_data[[dad_vars[1]]]
dad_w2 <- final_data[[dad_vars[2]]]
dad_w4 <- final_data[[dad_vars[3]]]

consolidate_one_parent <- function(w1, w2, w4) {
  stacked <- data.frame(wave = c(1, 2, 3), val = c(w1, w2, w4), stringsAsFactors = FALSE)
  pos_vals <- stacked$val[stacked$val > 0 & !is.na(stacked$val)]
  
  if (length(pos_vals) > 0) {
    pos_stacked <- stacked[stacked$val > 0 & !is.na(stacked$val), , drop = FALSE]
    earliest_idx <- which.min(pos_stacked$wave)
    return(as.numeric(pos_stacked$val[earliest_idx]))
  } else {
    neg_vals <- stacked$val[stacked$val < 0 & !is.na(stacked$val)]
    if (length(neg_vals) > 0) {
      priority_order <- c(-9, -8, -3, -2, -1)
      for (p in priority_order) {
        if (p %in% neg_vals) return(p)
      }
      return(-3)
    } else {
      return(-3)
    }
  }
}

cat("Consolidating mother's education...\n")
final_data$educdtlma <- consolidate_one_parent(mum_w1, mum_w2, mum_w4)

cat("Consolidating father's education...\n")
final_data$educdtlpa <- consolidate_one_parent(dad_w1, dad_w2, dad_w4)

cat("Consolidation complete.\n")

cat("\n--- Creating collapsed NVQ variables ---\n")

code_map <- c("1"=0, "2"=0, "3"=0, "4"=0, "5"=1, "6"=1, "7"=1, "8"=1, "9"=1, "10"=1, "11"=1, "12"=1, "13"=1, "14"=1, "15"=1, "16"=1, "17"=1, "18"=2, "19"=3, "20"=4)

create_collapsed <- function(x) {
  x_clean <- x
  x_num <- as.numeric(x)
  pos_mask <- x_num > 0 & !is.na(x_num)
  if (any(pos_mask)) {
    x_clean[pos_mask] <- code_map[as.character(x_num[pos_mask])]
  }
  return(x_clean)
}

final_data$educma <- create_collapsed(final_data$educdtlma)
final_data$educpa <- create_collapsed(final_data$educdtlpa)

final_data$educma[is.na(final_data$educma)] <- -3
final_data$educpa[is.na(final_data$educpa)] <- -3

cat("Collapsing complete.\n")

cat("\n--- Creating factor variables with labels ---\n")

# Create labels as a named vector - use strings for keys
educma_char_labels <- c(
  "-9" = "Refusal", 
  "-8" = "Don't know/insufficient information", 
  "-3" = "Not asked at the fieldwork stage/participated/interviewed", 
  "-2" = "Schedule not applicable/Script error/information lost", 
  "-1" = "Item not applicable", 
  "0" = "NVQ 4-5: degree-level qualifications and above", 
  "1" = "NVQ 1-3: sub-degree qualifications", 
  "2" = "None/entry: training programmes below NVQ level", 
  "3" = "Other: qualifications level unspecified", 
  "4" = "No qualifications mentioned"
)

dtlma_char_labels <- c(
  "-999" = "Missing - household data lost", 
  "-99" = "Mother not interviewed", 
  "-98" = "Mother not present", 
  "-94" = "Insufficient information", 
  "-92" = "Refused", 
  "-91" = "Not applicable", 
  "-1" = "Don't know", 
  "1" = "Higher Degree", 
  "2" = "First Degree", 
  "3" = "HE Diploma", 
  "4" = "HNC/HND/NVQ4", 
  "5" = "Teaching qualification, non-degree", 
  "6" = "Nursing qualification, non-degree", 
  "7" = "A Levels", 
  "8" = "OND/ONC", 
  "9" = "City and guilds part III, NVQ3", 
  "10" = "CSYS", 
  "11" = "Scottish Higher Grade", 
  "12" = "AS Level", 
  "13" = "Trade apprenticeship", 
  "14" = "City and guilds part II, NVQ2", 
  "15" = "GCSE grade A-C and equivalent", 
  "16" = "GCSE grade D-E and equivalent", 
  "17" = "City and guilds part I, NVQ1", 
  "18" = "Youth training, skill seekers", 
  "19" = "Qualification, level unspecified", 
  "20" = "No qualification mentioned"
)

# Convert to character, then label, then factor
educma_char <- as.character(final_data$educma)
educma_lab <- labelled::labelled(educma_char, labels = educma_char_labels)
educma_fact <- as.factor(educma_lab)

educpa_char <- as.character(final_data$educpa)
educpa_lab <- labelled::labelled(educpa_char, labels = educma_char_labels)
educpa_fact <- as.factor(educpa_lab)

educdtlma_char <- as.character(final_data$educdtlma)
educdtlma_lab <- labelled::labelled(educdtlma_char, labels = dtlma_char_labels)
educdtlma_fact <- as.factor(educdtlma_lab)

educdtlpa_char <- as.character(final_data$educdtlpa)
educdtlpa_lab <- labelled::labelled(educdtlpa_char, labels = dtlma_char_labels)
educdtlpa_fact <- as.factor(educdtlpa_lab)

cat("Factor variables with labels created.\n")

final_data$educma <- educma_fact
final_data$educpa <- educpa_fact
final_data$educdtlma <- educdtlma_fact
final_data$educdtlpa <- educdtlpa_fact

final_data <- final_data %>% select(NSID, educma, educpa, educdtlma, educdtlpa)

cat("Final variables:\n")
print(names(final_data))

cat("\nDataset dimensions:\n")
cat("  Rows:", nrow(final_data), "\n")
cat("  Columns:", ncol(final_data), "\n")

cat("\nVariable distributions:\n")
print(table(final_data$educma))
print(table(final_data$educpa))
print(table(final_data$educdtlma))
print(table(final_data$educdtlpa))

cat("\nSample data (first 10 rows):\n")
head(final_data, 10)

cat("\n--- Writing to CSV ---\n")
write_csv(final_data, output_file)
cat("Output written to:", output_file, "\n")
cat("Done!\n")