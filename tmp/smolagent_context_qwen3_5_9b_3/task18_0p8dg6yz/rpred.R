library(dplyr)
library(readr)

# Read the output file
output <- read_csv("data/output/cleaned_data.csv")

# Check summary stats
cat("Number of rows:", nrow(output), "\n")
cat("Number of columns:", ncol(output), "\n")

# Check unique values in ghqtl15
cat("\nUnique values in ghqtl15:")
print(unique(output$ghqtl15))

# Check what values are in the raw data
# Let's check wave2 data
wave2_data <- read_delim("data/input/wave_two_lsype_young_person_2020.tab", delim = "\t")

cat("\nGHQ item variables in wave2:\n")
for(var in c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", 
             "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", 
             "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")) {
  cat(var, ": min=", min(wave2_data[[var]], na.rm=TRUE), 
      "max=", max(wave2_data[[var]], na.rm=TRUE), "\n")
  cat(var, ": unique values:", length(unique(wave2_data[[var]])), "\n")
}

# Check what the item sum should be
gdq_items <- c("W2concenYP", "W2nosleepYP", "W2usefulYP", "W2decideYP", 
               "W2strainYP", "W2difficYP", "W2activYP", "W2probsYP", 
               "W2depressYP", "W2noconfYP", "W2wthlessYP", "W2happyYP")
cat("\nValid item values (1-4):\n")
for(var in gdq_items) {
  valid_vals <- unique(wave2_data[[var]][!(wave2_data[[var]] < 0 & !is.na(wave2_data[[var]])) & wave2_data[[var]] > 0])
  cat(var, ":", paste(valid_vals, collapse=", "), "\n")
}
'}}]