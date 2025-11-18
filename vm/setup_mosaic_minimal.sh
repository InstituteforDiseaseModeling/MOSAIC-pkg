#!/bin/bash
set -e

# Create timestamped log file
LOG_FILE="mosaic_install_minimal_$(date +%Y%m%d_%H%M%S).log"
exec > >(tee -a "$LOG_FILE")
exec 2>&1

echo "======================================"
echo "MOSAIC Minimal Setup (R pre-installed)"
echo "======================================"
echo "Logging to: $LOG_FILE"
echo ""

# Check OS version
OS_VERSION=$(lsb_release -rs 2>/dev/null || echo "unknown")
echo "Detected OS: Ubuntu $OS_VERSION"
echo ""

# Clean up previous failed installations
echo "[0/3] Cleaning up previous installation attempts..."
rm -rf ~/.local/share/r-miniconda 2>/dev/null || true
rm -rf ~/.virtualenvs 2>/dev/null || true
rm -rf ~/.conda 2>/dev/null || true
echo "Cleanup complete"
echo ""

# System libraries
echo "[1/3] Installing system dependencies..."
sudo apt-get update
sudo apt-get install -y \
  git \
  ca-certificates \
  build-essential \
  gfortran \
  cmake \
  gdal-bin libgdal-dev \
  libproj-dev libgeos-dev \
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
  zlib1g-dev \
  python3 python3-pip python3-venv python3-dev

# Note: cmake allows s2 R package to build Abseil C++ from source
# Note: libarrow-dev removed - arrow R package will compile from source
# This takes longer but avoids repository configuration issues

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

# MOSAIC R package
echo "[2/3] Installing MOSAIC R package..."
Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
  remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg', dependencies = TRUE, upgrade = 'never')"

# Python dependencies
echo "[3/3] Installing Python dependencies..."
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
  echo ""
  echo "Next steps:"
  echo "  1. Test MOSAIC: Rscript -e 'library(MOSAIC); MOSAIC::check_dependencies()'"
  echo "  2. View documentation: https://institutefordiseasemodeling.github.io/MOSAIC-pkg/"
  echo ""
  echo "Full installation log saved to: $LOG_FILE"
else
  echo ""
  echo "======================================"
  echo "Installation completed with errors"
  echo "======================================"
  echo ""
  echo "Troubleshooting steps:"
  echo "  1. Review the full log: cat $LOG_FILE"
  echo "  2. Re-run this script (it will clean up previous attempts)"
  echo "  3. Report issues: https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/issues"
  echo ""
  echo "Full installation log saved to: $LOG_FILE"
  exit 1
fi
