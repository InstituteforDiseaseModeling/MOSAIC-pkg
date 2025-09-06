#!/bin/bash

# Set the MOSAIC parent directory
MOSAIC_DIR="MOSAIC"

# Create the MOSAIC directory if it doesn't exist
if [ ! -d "$MOSAIC_DIR" ]; then
    mkdir "$MOSAIC_DIR"
    echo "Created directory: $MOSAIC_DIR"
fi

# Change to the MOSAIC directory
cd "$MOSAIC_DIR"

# Function to clone or update a repository
clone_or_update_repo() {
    REPO_NAME=$1
    REPO_URL=$2

    if [ ! -d "$REPO_NAME" ]; then
        git clone "$REPO_URL"
        echo "Cloned $REPO_NAME repository."
    else
        echo "$REPO_NAME repository already exists. Pulling latest changes."
        cd "$REPO_NAME"
        git pull
        cd ..
    fi
}

# Clone or update the MOSAIC-data repository
clone_or_update_repo "MOSAIC-data" "git@github.com:InstituteforDiseaseModeling/MOSAIC-data.git"

# Clone or update the MOSAIC-pkg repository
clone_or_update_repo "MOSAIC-pkg" "git@github.com:InstituteforDiseaseModeling/MOSAIC-pkg.git"

# Clone or update the MOSAIC-docs repository
clone_or_update_repo "MOSAIC-docs" "git@github.com:InstituteforDiseaseModeling/MOSAIC-docs.git"

# Clone or update the ees-cholera-mapping repository
clone_or_update_repo "ees-cholera-mapping" "git@github.com:InstituteforDiseaseModeling/ees-cholera-mapping.git"

echo "MOSAIC project setup complete."
