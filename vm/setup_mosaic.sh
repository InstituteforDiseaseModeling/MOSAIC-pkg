#!/bin/bash
set -e  # Exit on any error

echo "======================================"
echo "MOSAIC VM Setup Script"
echo "======================================"

# Update system
echo "[1/7] Updating system packages..."
sudo apt-get update
sudo apt-get upgrade -y

# Install essential build tools
echo "[2/7] Installing essential build tools..."
sudo apt-get install -y \
  git \
  ca-certificates \
  build-essential \
  gfortran

# Install R and dependencies
echo "[3/7] Installing R (>= 4.1.1)..."
sudo apt-get install -y software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt-get update
sudo apt-get install -y r-base r-base-dev

# Install system dependencies for geospatial operations and R packages
echo "[4/7] Installing system libraries (GDAL, PROJ, GEOS, HDF5)..."
sudo apt-get install -y \
  gdal-bin libgdal-dev \
  libproj-dev proj-bin \
  libgeos-dev libgeos++-dev \
  libudunits2-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  libhdf5-dev \
  zlib1g-dev

# Note: libarrow-dev removed - arrow R package will compile from source
# This takes longer but avoids repository configuration issues

# Install Python 3.9+
echo "[5/7] Installing Python 3.9+..."
sudo apt-get install -y \
  python3 \
  python3-pip \
  python3-venv \
  python3-dev

# Verify Python version
PYTHON_VERSION=$(python3 --version 2>&1 | grep -oP '\d+\.\d+' | head -1)
PYTHON_MAJOR=$(echo $PYTHON_VERSION | cut -d. -f1)
PYTHON_MINOR=$(echo $PYTHON_VERSION | cut -d. -f2)

if [ "$PYTHON_MAJOR" -lt 3 ] || ([ "$PYTHON_MAJOR" -eq 3 ] && [ "$PYTHON_MINOR" -lt 9 ]); then
  echo "WARNING: Python $PYTHON_VERSION detected, but >= 3.9 required"
  echo "Attempting to install Python 3.9..."
  sudo add-apt-repository -y ppa:deadsnakes/ppa
  sudo apt-get update
  sudo apt-get install -y python3.9 python3.9-venv python3.9-dev
  sudo update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.9 1
  echo "Python 3.9 installed and set as default"
else
  echo "Python $PYTHON_VERSION detected (OK)"
fi

# Install R packages system-wide (non-interactive)
echo "[6/7] Installing MOSAIC R package..."
sudo Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
  if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes'); \
  remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg', dependencies = TRUE, upgrade = 'always')"

# Install Python dependencies
echo "[7/7] Installing Python dependencies..."
sudo Rscript -e "MOSAIC::install_dependencies()"

# Verify installation
echo ""
echo "Verifying installation..."
Rscript -e "
  library(MOSAIC)
  result <- tryCatch({
    MOSAIC::check_dependencies()
    TRUE
  }, error = function(e) {
    cat('ERROR:', e\$message, '\n')
    FALSE
  })
  if (!result) quit(status = 1)
"

if [ $? -eq 0 ]; then
  echo ""
  echo "======================================"
  echo "Installation complete and verified!"
  echo "======================================"
else
  echo ""
  echo "======================================"
  echo "Installation completed with errors"
  echo "Please check the output above"
  echo "======================================"
  exit 1
fi
