library(haven)
library(dplyr)
library(readr)

# Load all files - read the specific language columns as numeric directly
w1 <- read_delim("data/input/wave_one_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = "c", W1englangYP = "d", .default = "c"))
w2 <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t", col_types = cols(NSID = "c", W2EnglangYP = "d", .default = "c"))
w3 <- read_delim("data/input/wave_three_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = "c", W3englangHH = "d", .default = "c"))
w4 <- read_delim("data/input/wave_four_lsype_family_background_2020.tab", delim = "\t", col_types = cols(NSID = "c", W4EngLangHH = "d", .default = "c"))

print("Files loaded")
print(paste("w1 rows:", nrow(w1)))
print(paste("w2 rows:", nrow(w2)))
print(paste("w3 rows:", nrow(w3)))
print(paste("w4 rows:", nrow(w4)))

print(paste("w1 W1englangYP class:", class(w1$W1englangYP)))
print(paste("w1 W1englangYP min/max:", min(w1$W1englangYP, na.rm=TRUE), max(w1$W1englangYP, na.rm=TRUE)))

# Recode function - source -94 maps to -2, source -1 maps to -8
recode_lang <- function(x) {
  result <- as.numeric(x)
  result[is.na(result)] <- -3
  result[result == -94] <- -2
  result[result == -1] <- -8
  result
}

# Apply recoding
w1$W1englangYP <- recode_lang(w1$W1englangYP)
w2$W2EnglangYP <- recode_lang(w2$W2EnglangYP)
w3$W3englangHH <- recode_lang(w3$W3englangHH)
w4$W4EngLangHH <- recode_lang(w4$W4EngLangHH)

# Merge all datasets by NSID
merged <- full_join(w1, w2, by = "NSID")
merged <- full_join(merged, w3, by = "NSID")
merged <- full_join(merged, w4, by = "NSID")

print(paste("Merged rows:", nrow(merged)))

# Create lang variable using earliest valid positive response
merged <- merged %>%
  mutate(
    lang = case_when(
      !is.na(W1englangYP) & W1englangYP > 0 ~ W1englangYP,
      !is.na(W2EnglangYP) & W2EnglangYP > 0 ~ W2EnglangYP,
      !is.na(W3englangHH) & W3englangHH > 0 ~ W3englangHH,
      !is.na(W4EngLangHH) & W4EngLangHH > 0 ~ W4EngLangHH,
      TRUE ~ -3
    )
  ) %>%
  as_tibble()

print(paste("Lang unique values:", paste(unique(merged$lang), collapse=", ")))

# Keep only NSID and lang
merged <- merged %>%
  select(NSID, lang)

# Create factor with appropriate labels
merged$lang <- factor(merged$lang, 
                     levels = c("-3", "-8", "-2", "-9", "-1", "1", "2", "3", "4"),
                     labels = c("Not asked", "Don't know", "Not applicable", "Refusal", "Item not applicable", 
                               "Yes - English only", "Yes - English first/main", "No - another language", "Bilingual"))

print("Factor created")
print(table(merged$lang))

# Write output
write_csv(merged, "data/output/cleaned_data.csv")
print("Data written successfully")
print(head(merged))
}