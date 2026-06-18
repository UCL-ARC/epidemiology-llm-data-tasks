# Load required libraries
library(dplyr)
library(readr)
library(tidyr)

# Create output directory if it doesn't exist
dir.create("data/output", showWarnings = FALSE)

# Load all files from metadata
df_wave1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df_wave2 <- read_delim("data/input/wave_two_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df_wave3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", show_col_types = FALSE)
df_wave4 <- read_delim("data/input/wave_four_lsype_young_person_2020.tab", delim = "\t", show_col_types = FALSE)
df_wave8 <- read_delim("data/input/ns8_2015_derived.tab", delim = "\t", show_col_types = FALSE)
df_wave9_derived <- read_delim("data/input/ns9_2022_derived_variables.tab", delim = "\t", show_col_types = FALSE)
df_wave9_main <- read_delim("data/input/ns9_2022_main_interview.tab", delim = "\t", show_col_types = FALSE)

# Rename wave-specific columns before merging to avoid overwriting
df_wave2 <- df_wave2 %>%
  rename(urbind15 = urbind, gor15 = gor)

df_wave3 <- df_wave3 %>%
  rename(urbind16 = urbind, gor16 = gor)

# Full join all datasets by NSID
df <- df_wave1 %>%
  full_join(df_wave2, by = "NSID") %>%
  full_join(df_wave3, by = "NSID") %>%
  full_join(df_wave4, by = "NSID") %>%
  full_join(df_wave8, by = "NSID") %>%
  full_join(df_wave9_derived, by = "NSID") %>%
  full_join(df_wave9_main, by = "NSID")

print(paste("Total rows after merge:", nrow(df)))

# Helper function to convert missing values
code_missing <- function(x, label_map) {
  result <- x
  for (code in names(label_map)) {
    label <- label_map[[code]]
    val <- as.numeric(code)
    if (grepl("Refusal", label, ignore.case = TRUE)) {
      result[result == val] <- -9
    } else if (grepl("Don't know", label, ignore.case = TRUE) || 
               grepl("Insufficient information", label, ignore.case = TRUE)) {
      result[result == val] <- -8
    } else if (grepl("Prefer", label, ignore.case = TRUE)) {
      result[result == val] <- -7
    } else if (grepl("Not asked", label, ignore.case = TRUE)) {
      result[result == val] <- -3
    } else if (grepl("Not applicable", label, ignore.case = TRUE) ||
               grepl("Not asked at the fieldwork stage", label, ignore.case = TRUE)) {
      result[result == val] <- -3
    } else if (grepl("Schedule not applicable|script error|information lost|faulty|missing", label, ignore.case = TRUE)) {
      result[result == val] <- -2
    }
  }
  result[is.na(result)] <- -3
  return(result)
}

# Define value labels for wave2/3 urbind
urbind_labels_w2w3 <- c("-999" = "Schedule not applicable", "-998" = "Script error", "-997" = "Prefer not to say", 
                        "-995" = "Information lost", "-94" = "Insufficient information", 
                        "-1" = "Item not applicable")

gor_labels_w2w3 <- c("-999" = "Schedule not applicable", "-998" = "Script error", "-997" = "Prefer not to say",
                     "-995" = "Information lost", "-94" = "Insufficient information", "-1" = "Item not applicable")

w8dgor_labels <- c("-9" = "Refused", "-8" = "Insufficient information", "-1" = "Not applicable")
w9drgn_labels <- c("-9" = "Refused", "-8" = "Insufficient information", "-1" = "Not applicable")
w9nationres_labels <- c("-9" = "Refused", "-8" = "Don't know", "-3" = "Not asked at fieldwork stage", "-1" = "Not applicable")

# Create derived variables
regub15 <- code_missing(df$urbind15, urbind_labels_w2w3)
regub16 <- code_missing(df$urbind16, urbind_labels_w2w3)
regov15 <- code_missing(df$gor15, gor_labels_w2w3)
regov16 <- code_missing(df$gor16, gor_labels_w2w3)
regor25 <- code_missing(df$W8DGOR, w8dgor_labels)
regor32 <- code_missing(df$W9DRGN, w9drgn_labels)
regint32 <- code_missing(df$W9NATIONRES, w9nationres_labels)

# Create output dataframe with only NSID and derived variables
output_df <- df %>%
  select(NSID) %>%
  mutate(
    regub15 = regub15,
    regub16 = regub16,
    regov15 = regov15,
    regov16 = regov16,
    regor25 = regor25,
    regor32 = regor32,
    regint32 = regint32
  )

# Write output
write_csv(output_df, "data/output/cleaned_data.csv")

print("Output written successfully!")
print(head(output_df[, c("NSID", "regub15", "regub16", "regov15", "regov16", "regor25", "regor32", "regint32")]))
