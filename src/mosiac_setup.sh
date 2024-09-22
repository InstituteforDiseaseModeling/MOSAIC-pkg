#!/bin/bash

# Script to set up the MOSAIC project directory and clone the required repositories

# Set the MOSAIC parent directory
MOSAIC_DIR="MOSAIC"

# Create the MOSAIC directory if it doesn't exist
if [ ! -d "$MOSAIC_DIR" ]; then
    mkdir "$MOSAIC_DIR"
    echo "Created directory: $MOSAIC_DIR"
fi

# Change to the MOSAIC directory
cd "$MOSAIC_DIR"

# Clone the MOSAIC-data repository
if [ ! -d "MOSAIC-data" ]; then
    git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-data.git
    echo "Cloned MOSAIC-data repository."
else
    echo "MOSAIC-data repository already exists."
fi

# Clone the MOSAIC-pkg repository
if [ ! -d "MOSAIC-pkg" ]; then
    git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg.git
    echo "Cloned MOSAIC-pkg repository."
else
    echo "MOSAIC-pkg repository already exists."
fi

# Clone the MOSAIC-docs repository
if [ ! -d "MOSAIC-docs" ]; then
    git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-docs.git
    echo "Cloned MOSAIC-docs repository."
else
    echo "MOSAIC-docs repository already exists."
fi

echo "MOSAIC project setup complete."

