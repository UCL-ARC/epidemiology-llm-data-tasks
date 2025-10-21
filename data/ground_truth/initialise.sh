#!/bin/bash

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to the script directory
cd "$SCRIPT_DIR"

# Set variables
JSON_FILE="metadata.json"
SOURCE_DIR="../../raw_data/UKDA-5545-tab/tab/safeguarded_eul"  # Update this to your actual source directory
INPUT_DIR="data/input"

for test in *; do

    # Skip if test name is not test + number
    if [[ $test =~ ^test[0-9]+$ ]]; then
        echo "----------------------------------------------------------"
        echo "Processing test directory: $test"

        cd "$SCRIPT_DIR/$test"

        # Check if JSON file exists
        if [ ! -f "$JSON_FILE" ]; then
            echo "Error: $JSON_FILE not found!"
            exit 1
        fi

        # Extract all keys (filenames) from JSON and copy corresponding files
        echo "Reading filenames from $JSON_FILE..."

        # Use jq to extract top-level keys (filenames) and iterate over them
        jq -r 'keys[]' "$JSON_FILE" | while read -r filename; do
            echo "Processing: $filename"
            
            # Check if source file exists
            source_file="${SOURCE_DIR}/${filename}"

            if [ -n "$source_file" ]; then
                echo "Copying $source_file to $INPUT_DIR/"
                cp "$source_file" "$INPUT_DIR/"
            else
                echo "Initialisation failed: No file found for $filename in $SOURCE_DIR"
                exit 1
            fi
        done

        echo "File copying completed."

        # Run the R script to generate outputs
        echo "Current directory before running R script: $(pwd)"

        if [ -f "rtruth.R" ]; then
            echo "Running rtruth.R from directory: $(pwd)"
            Rscript rtruth.R
            if [ $? -eq 0 ]; then
                echo "R script execution completed successfully."
            else
                echo "Initialisation failed: R script execution failed!"
                exit 1
            fi
        else
            echo "Initialisation failed: rtruth.R not found!"
            exit 1
        fi
    fi

    echo "$test initialised successfully."

done

echo "Initialisation completed successfully."
