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
# Install to user library to avoid permission issues
# This is more portable and doesn't require sudo
mkdir -p ~/R/library
Rscript -e "
  .libPaths(c('~/R/library', .libPaths()))
  options(repos = c(CRAN = 'https://cloud.r-project.org'))
  remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg',
                         dependencies = TRUE,
                         upgrade = 'never',
                         lib = '~/R/library')
"

# Python dependencies
echo "[3/4] Installing Python dependencies..."
Rscript -e "
  .libPaths(c('~/R/library', .libPaths()))
  MOSAIC::install_dependencies(force = TRUE)
"

# Configure R wrapper for Ubuntu 20.04 GLIBCXX compatibility
echo "[4/4] Configuring R wrapper for Ubuntu 20.04 compatibility..."
if [ "$OS_VERSION" = "20.04" ]; then
  echo "  Creating r-mosaic-R and r-mosaic-Rscript wrappers..."

  # Create ~/bin directory
  mkdir -p ~/bin

  # Create R wrapper script with auto-detection of libstdc++ location
  cat > ~/bin/r-mosaic-R <<'EOF'
#!/usr/bin/env bash
# Find a modern libstdc++ inside common reticulate env locations
candidates=(
  "$HOME/.virtualenvs/r-mosaic/lib/libstdc++.so.6"
  "$HOME/.local/share/r-miniconda/envs/r-mosaic/lib/libstdc++.so.6"
  "$HOME/.local/share/r-miniconda/envs/r-reticulate/lib/libstdc++.so.6"
)
for f in "${candidates[@]}"; do
  if [ -f "$f" ] && strings "$f" 2>/dev/null | grep -q "GLIBCXX_3\.4\.29"; then
    export LD_PRELOAD="$f${LD_PRELOAD:+:$LD_PRELOAD}"
    export LD_LIBRARY_PATH="$(dirname "$f")${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    break
  fi
done
# Ensure conda's lib directory is first for libexpat and other Python deps
if [ -d "$HOME/.virtualenvs/r-mosaic/lib" ]; then
  export LD_LIBRARY_PATH="$HOME/.virtualenvs/r-mosaic/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
fi
# Set R library path to include user library
export R_LIBS_USER="$HOME/R/library"
# If we later want to hard-pin Python for reticulate:
[ -x "$HOME/.virtualenvs/r-mosaic/bin/python" ] && export RETICULATE_PYTHON="$HOME/.virtualenvs/r-mosaic/bin/python"
exec R "$@"
EOF
  chmod +x ~/bin/r-mosaic-R

  # Create Rscript wrapper with same logic
  cat > ~/bin/r-mosaic-Rscript <<'EOF'
#!/usr/bin/env bash
# Same preload logic for Rscript
candidates=(
  "$HOME/.virtualenvs/r-mosaic/lib/libstdc++.so.6"
  "$HOME/.local/share/r-miniconda/envs/r-mosaic/lib/libstdc++.so.6"
  "$HOME/.local/share/r-miniconda/envs/r-reticulate/lib/libstdc++.so.6"
)
for f in "${candidates[@]}"; do
  if [ -f "$f" ] && strings "$f" 2>/dev/null | grep -q "GLIBCXX_3\.4\.29"; then
    export LD_PRELOAD="$f${LD_PRELOAD:+:$LD_PRELOAD}"
    export LD_LIBRARY_PATH="$(dirname "$f")${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    break
  fi
done
# Ensure conda's lib directory is first for libexpat and other Python deps
if [ -d "$HOME/.virtualenvs/r-mosaic/lib" ]; then
  export LD_LIBRARY_PATH="$HOME/.virtualenvs/r-mosaic/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
fi
# Set R library path to include user library
export R_LIBS_USER="$HOME/R/library"
[ -x "$HOME/.virtualenvs/r-mosaic/bin/python" ] && export RETICULATE_PYTHON="$HOME/.virtualenvs/r-mosaic/bin/python"
exec Rscript "$@"
EOF
  chmod +x ~/bin/r-mosaic-Rscript

  # Ensure launchers are first on PATH for this session and future shells
  export PATH="$HOME/bin:$PATH"
  if ! grep -q 'export PATH="$HOME/bin:$PATH"' ~/.bashrc; then
    echo 'export PATH="$HOME/bin:$PATH"' >> ~/.bashrc
  fi

  echo "  ✓ Created r-mosaic-R wrapper at ~/bin/r-mosaic-R"
  echo "  ✓ Created r-mosaic-Rscript wrapper at ~/bin/r-mosaic-Rscript"
  echo "  ✓ Added ~/bin to PATH in ~/.bashrc"
else
  echo "  Skipping wrapper creation (only needed for Ubuntu 20.04)"
fi

# Verify installation
echo ""
echo "Verifying installation..."

# Use wrapper script on Ubuntu 20.04 if it was created
if [ "$OS_VERSION" = "20.04" ] && [ -x "$HOME/bin/r-mosaic-Rscript" ]; then
  export PATH="$HOME/bin:$PATH"
  R_CMD="r-mosaic-Rscript"
else
  R_CMD="Rscript"
fi

$R_CMD -e "
  .libPaths(c('~/R/library', .libPaths()))
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
  echo "  - MOSAIC R package: Installed to ~/R/library"
  echo "  - Python environment: ~/.virtualenvs/r-mosaic"
  echo ""

  # Set R_LIBS_USER permanently
  if ! grep -q "R_LIBS_USER" ~/.Renviron 2>/dev/null; then
    echo "R_LIBS_USER=~/R/library" >> ~/.Renviron
    echo "  ✓ Added R_LIBS_USER to ~/.Renviron"
  fi
  echo ""

  if [ "$OS_VERSION" = "20.04" ]; then
    echo "Ubuntu 20.04 detected - Using r-mosaic wrappers:"
    echo "  - 'r-mosaic-R' and 'r-mosaic-Rscript' preload compatible C++ libraries"
    echo "  - This fixes GLIBCXX version conflicts with Python packages"
    echo "  - Use wrappers instead of 'R'/'Rscript' for MOSAIC work"
    echo ""
    echo "Next steps:"
    echo "  1. Reload shell: source ~/.bashrc"
    echo "  2. Test wrapper: r-mosaic-Rscript -e 'library(MOSAIC); MOSAIC::check_dependencies()'"
    echo "  3. Batch scripts: r-mosaic-Rscript your_script.R"
    echo "  4. Interactive session: r-mosaic-R"
  else
    echo "Next steps:"
    echo "  1. Test MOSAIC: Rscript -e 'library(MOSAIC); MOSAIC::check_dependencies()'"
  fi

  echo "  5. View documentation: https://institutefordiseasemodeling.github.io/MOSAIC-pkg/"
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
