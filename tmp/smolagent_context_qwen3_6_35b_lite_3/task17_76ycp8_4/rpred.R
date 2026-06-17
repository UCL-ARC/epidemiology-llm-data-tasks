library(readr)

# Read the output file to verify
df <- read_csv('data/output/cleaned_data.csv')
print(head(df))
print(paste('Number of rows:', nrow(df)))
print(paste('Number of columns:', ncol(df)))
print(names(df))

# Check distributions
print('=== imd15 ===')
print(table(df$imd15, useNA = 'ifany'))

print('=== imd16 ===')
print(table(df$imd16, useNA = 'ifany'))

print('=== imd32 ===')
print(table(df$imd32, useNA = 'ifany'))
