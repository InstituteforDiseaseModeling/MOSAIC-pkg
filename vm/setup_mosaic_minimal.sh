#!/bin/bash
set -e

echo "======================================"
echo "MOSAIC Minimal Setup (R pre-installed)"
echo "======================================"

# System libraries
echo "[1/3] Installing system dependencies..."
sudo apt-get update
sudo apt-get install -y \
  gdal-bin libgdal-dev \
  libproj-dev libgeos-dev \
  libudunits2-dev \
  python3 python3-pip python3-venv

# MOSAIC R package
echo "[2/3] Installing MOSAIC R package..."
Rscript -e "remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg')"

# Python dependencies
echo "[3/3] Installing Python dependencies..."
Rscript -e "MOSAIC::install_dependencies()"

echo "======================================"
echo "Installation complete!"
echo "======================================"
