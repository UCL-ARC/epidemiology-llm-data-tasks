#!/bin/bash

START_DIR=$(pwd)
GROUND_TRUTH_DIR="ground_truth"
JSON_FILE="metadata.json"
SOURCE_DIR="data/UKDA-5545-tab/tab/safeguarded_eul"  # This should be changed as needed

# Verify ground truth directory exists
if [ ! -d "$GROUND_TRUTH_DIR" ]; then
    echo "Error: Ground truth directory '$GROUND_TRUTH_DIR' not found."
    exit 1
fi

# Array to track failed samples
FAILED_SAMPLES=()

# Iterate through sample directories
for sample_dir in "$GROUND_TRUTH_DIR"/sample[0-9]*; do
    [ -d "$sample_dir" ] || continue  # skip if not a directory

    echo "----------------------------------------------------------"
    echo "Processing: $sample_dir"

    metadata="$sample_dir/$JSON_FILE"
    r_script="$sample_dir/rtruth.R"
    input_dir="$sample_dir/data/input"

    # Check JSON file
    if [ ! -f "$metadata" ]; then
        echo "Error: $JSON_FILE not found in $sample_dir"
        echo "Skipping $sample_dir"
        FAILED_SAMPLES+=("$sample_dir")
        continue
    fi
    
    echo "Reading filenames from $metadata..."

    # Copy files listed in JSON
    jq -r 'keys[]' "$metadata" | while read -r filename; do
        [ -z "$filename" ] && continue
        source_file="$START_DIR/$SOURCE_DIR/$filename"

        if [ -f "$source_file" ]; then
            echo "Copying: $source_file -> $input_dir/"
            cp "$source_file" "$input_dir/" || echo "Copy failed for $filename"
        else
            echo "Warning: No source file found for $filename"
            echo "Skipping $sample_dir"
            FAILED_SAMPLES+=("$sample_dir")
            continue 2
        fi
    done

    echo "File copying completed for $sample_dir"

    # Run R script *from inside* the sample directory
    if [ -f "$r_script" ]; then
        echo "Running R script"
        if ( cd "$sample_dir" && Rscript rtruth.R ); then
            echo "R script completed successfully for $sample_dir"
        else
            echo "R script failed for $sample_dir"
            echo "Skipping $sample_dir"
            FAILED_SAMPLES+=("$sample_dir")
            continue
        fi
    else
        echo "Error: rtruth.R not found in $sample_dir"
        echo "Skipping $sample_dir"
        FAILED_SAMPLES+=("$sample_dir")
        continue
    fi

    echo "$sample_dir initialised successfully."

done

echo "----------------------------------------------------------"
echo "Initialisation process finished for all samples."

# Report failed samples
if [ ${#FAILED_SAMPLES[@]} -eq 0 ]; then
    echo "All samples processed successfully."
else
    echo "The following samples failed:"
    for failed in "${FAILED_SAMPLES[@]}"; do
        echo "  - $failed"
    done
fi