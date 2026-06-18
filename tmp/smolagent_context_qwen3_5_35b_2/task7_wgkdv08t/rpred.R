library(readr)

# Read the output file
cleaned <- read_csv('data/output/cleaned_data.csv', show_col_types = FALSE)

# Check the distribution of values
print('educaim17 distribution:')
print(table(cleaned$educaim17, useNA = 'ifany'))

print('educaim19 distribution:')
print(table(cleaned$educaim19, useNA = 'ifany'))

print('educaim20 distribution:')
print(table(cleaned$educaim20, useNA = 'ifany'))

print('educaim25 distribution:')
print(table(cleaned$educaim25, useNA = 'ifany'))

print('educaim32 distribution:')
print(table(cleaned$educaim32, useNA = 'ifany'))

# Check a few rows
print('First 10 rows:')
print(head(cleaned, 10))

# Check how many have NA in educaim17 vs educaim19
print(paste('educaim17 NAs:', sum(is.na(cleaned$educaim17))))
print(paste('educaim19 NAs:', sum(is.na(cleaned$educaim19))))
print(paste('educaim20 NAs:', sum(is.na(cleaned$educaim20))))
print(paste('educaim25 NAs:', sum(is.na(cleaned$educaim25))))
print(paste('educaim32 NAs:', sum(is.na(cleaned$educaim32))))

# Write first few rows to file to verify
write_csv(cleaned[1:20,], 'data/output/sample_output.csv')
print('Sample written to data/output/sample_output.csv')
