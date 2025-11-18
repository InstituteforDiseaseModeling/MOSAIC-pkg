#!/bin/bash
set -e  # Exit on any error

# Create timestamped log file
LOG_FILE="mosaic_install_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG_FILE")
exec 2>&1

echo "======================================"
echo "MOSAIC VM Setup Script"
echo "======================================"
echo "Logging to: $LOG_FILE"
echo ""

# Check OS version
OS_VERSION=$(lsb_release -rs 2>/dev/null || echo "unknown")
OS_CODENAME=$(lsb_release -cs 2>/dev/null || echo "unknown")
echo "Detected OS: Ubuntu $OS_VERSION ($OS_CODENAME)"

if [ "$OS_VERSION" != "20.04" ]; then
  echo "WARNING: This script is optimized for Ubuntu 20.04"
  echo "You are running Ubuntu $OS_VERSION - installation may encounter issues"
  echo "Press Ctrl+C to cancel, or wait 5 seconds to continue..."
  sleep 5
fi
echo ""

# Clean up previous failed installations
echo "[0/7] Cleaning up previous installation attempts..."
rm -rf ~/.local/share/r-miniconda 2>/dev/null || true
rm -rf ~/.virtualenvs 2>/dev/null || true
rm -rf ~/.conda 2>/dev/null || true
sudo rm -rf /root/.local/share/r-miniconda 2>/dev/null || true
sudo rm -rf /root/.virtualenvs 2>/dev/null || true
echo "Cleanup complete"
echo ""

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
  gfortran \
  cmake

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

# Note: cmake (installed above) allows s2 R package to build Abseil from source
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

# Install critical geospatial R packages first (these often fail)
echo "[6/7] Installing critical geospatial R packages..."
sudo Rscript -e "
options(repos = c(CRAN = 'https://cloud.r-project.org'))

# Install remotes
if (!requireNamespace('remotes', quietly = TRUE)) {
  install.packages('remotes')
}

# Install geospatial packages one by one with error checking
cat('Installing sf...\n')
if (!requireNamespace('sf', quietly = TRUE)) {
  install.packages('sf', configure.args = '--with-proj-lib=/usr/lib')
  if (!requireNamespace('sf', quietly = TRUE)) {
    stop('Failed to install sf package')
  }
}

cat('Installing terra (version compatible with GEOS 3.8)...\n')
if (!requireNamespace('terra', quietly = TRUE)) {
  # Ubuntu 20.04 has GEOS 3.8, but current terra requires GEOS 3.10+
  # Install terra 1.7-71 (last version compatible with GEOS 3.8)
  remotes::install_version('terra', version = '1.7.71', repos = 'https://cloud.r-project.org')
  if (!requireNamespace('terra', quietly = TRUE)) {
    stop('Failed to install terra package')
  }
}

cat('Installing raster (version compatible with terra 1.7-71)...\n')
if (!requireNamespace('raster', quietly = TRUE)) {
  # Latest raster requires terra >= 1.8.5, but we have terra 1.7-71
  # Install raster 3.6-26 (compatible with terra 1.7-71)
  remotes::install_version('raster', version = '3.6.26', repos = 'https://cloud.r-project.org')
  if (!requireNamespace('raster', quietly = TRUE)) {
    stop('Failed to install raster package')
  }
}

cat('Geospatial packages installed successfully\n')
"

# Now install MOSAIC with all dependencies
echo "[7/7] Installing MOSAIC R package..."
sudo Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
  remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg', dependencies = TRUE, upgrade = 'never')"

# Install Python dependencies (run as user, not root)
echo "[7/7] Installing Python dependencies..."
Rscript -e "MOSAIC::install_dependencies(force = TRUE)"

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
  echo ""
  echo "Installation Summary:"
  echo "  - R version: $(R --version | head -1)"
  echo "  - Python version: $(python3 --version)"
  echo "  - MOSAIC R package: $(Rscript -e "cat(as.character(packageVersion('MOSAIC')))" 2>/dev/null)"
  echo "  - Python environment: ~/.virtualenvs/r-mosaic"
  echo "  - Conda location: ~/.local/share/r-miniconda"
  echo ""
  echo "Next steps:"
  echo "  1. Test MOSAIC: Rscript -e 'library(MOSAIC); MOSAIC::check_dependencies()'"
  echo "  2. View documentation: https://institutefordiseasemodeling.github.io/MOSAIC-pkg/"
  echo "  3. Report issues: https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues"
  echo ""
  echo "Full installation log saved to: $LOG_FILE"
else
  echo ""
  echo "======================================"
  echo "Installation completed with errors"
  echo "======================================"
  echo ""
  echo "Please check the error messages above and try the following:"
  echo "  1. Review the full log: cat $LOG_FILE"
  echo "  2. Check system requirements: Ubuntu 20.04, 8GB RAM, 20GB disk"
  echo "  3. Re-run this script (it will clean up previous attempts)"
  echo "  4. Report issues: https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues"
  echo ""
  echo "Full installation log saved to: $LOG_FILE"
  exit 1
fi
