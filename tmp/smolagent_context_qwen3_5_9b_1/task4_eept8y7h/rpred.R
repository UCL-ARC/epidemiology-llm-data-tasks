library(dplyr)
library(readr)

# Check the output file
clean_data <- read_csv('data/output/cleaned_data.csv')

# Check the structure
print('Variables containing sori:')
print(grep('sori', colnames(clean_data), value = TRUE))

# Check the sori variables structure
print('sori19 sample:')
print(head(clean_data$sori19))
print('sori20 sample:')
print(head(clean_data$sori20))
print('sori25 sample:')
print(head(clean_data$sori25))
print('sori32 sample:')
print(head(clean_data$sori32))

# Check value counts for each
print('sori19 value counts:')
print(table(clean_data$sori19))
print('sori20 value counts:')
print(table(clean_data$sori20))
print('sori25 value counts:')
print(table(clean_data$sori25))
print('sori32 value counts:')
print(table(clean_data$sori32))

print('Script verification completed')
