#!/usr/bin/env bash
# setup_mosaic.sh — install JAGS, SSL, PROJ, GDAL, Python, etc. for MOSAIC
# Usage: bash setup_mosaic.sh

set -euo pipefail

# Detect OS
OS="$(uname -s)"
echo "Detected OS: $OS"

if [[ "$OS" == "Linux" ]]; then
  echo "→ Installing system dependencies via apt on Linux"
  sudo apt-get update
  sudo apt-get install -y \
    jags \
    libssl-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    python3-dev

elif [[ "$OS" == "Darwin" ]]; then
  echo "→ Installing system dependencies via Homebrew on macOS"
  # ensure Homebrew is available
  if ! command -v brew &>/dev/null; then
    echo "Homebrew not found—installing..."
    /bin/bash -c \
      "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi

  brew update
  brew install jags openssl@3 udunits python@3.12

  # compile‐time flags for R packages that use OpenSSL & UDUNITS
  export PKG_CONFIG_PATH="$(brew --prefix openssl@3)/lib/pkgconfig:$(brew --prefix udunits)/lib/pkgconfig:$PKG_CONFIG_PATH"
  export LDFLAGS="-L$(brew --prefix openssl@3)/lib -L$(brew --prefix udunits)/lib $LDFLAGS"
  export CPPFLAGS="-I$(brew --prefix openssl@3)/include -I$(brew --prefix udunits)/include $CPPFLAGS"

  # “keg-only” JAGS needs to be symlinked where rjags will look
  JAGS_PREFIX="$(brew --prefix jags)"
  sudo mkdir -p /usr/local/lib /usr/local/include/JAGS /usr/local/lib/JAGS
  sudo ln -sfn "$JAGS_PREFIX/lib/libjags.4.dylib"  /usr/local/lib/libjags.4.dylib
  sudo ln -sfn "$JAGS_PREFIX/include/JAGS"         /usr/local/include/JAGS
  sudo ln -sfn "$JAGS_PREFIX/lib/JAGS/modules-4"   /usr/local/lib/JAGS/modules-4

  echo "JAGS modules:"
  ls -l /usr/local/lib/JAGS/modules-4

else
  echo "ERROR: Unsupported OS: $OS" >&2
  exit 1
fi

echo "System dependencies installed successfully."

# ─── Now install R-level dependencies and verify MOSAIC can load ───

# Install all Depends & Imports from DESCRIPTION
Rscript -e 'if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")'
Rscript -e 'remotes::install_deps(dependencies = c("Depends","Imports"))'

# Check that the package loads
Rscript -e 'library(MOSAIC)'
