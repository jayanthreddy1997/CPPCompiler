#!/bin/bash

# Compile cpp_compiler if needed
make clean
make

# Create an output directory if it doesn't exist
mkdir -p output

# Loop through each file in the test directory
for file in test/*; do
    # Extract the filename without the path and extension
    filename=$(basename -- "$file")
    filename_no_ext="${filename%.*}"

    # Compile and run cpp_compiler with the current test file
    ./cpp_compiler < "$file" > "output/$filename_no_ext.out" 2>&1

    # Display the output file for this test file
    echo "Output for $filename:"
    cat "output/$filename_no_ext.out"
    echo "-----------------------------------------"
done

