#!/bin/bash
set -e

# ==============================================================================
# MOSAIC ETH Production Run - VM Launch Script
# ==============================================================================
# This script:
# - Sets up minimal required directory structure
# - Clones necessary GitHub repositories
# - Configures environment for Ubuntu 20.04 compatibility
# - Launches the ETH calibration run with proper logging
# - Supports tmux for long-running sessions
# ==============================================================================

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT_DIR="$HOME/MOSAIC/output/ETH"
LOG_FILE="${OUTPUT_DIR}/mosaic_ETH_${TIMESTAMP}.log"
TMUX_SESSION="mosaic-eth"

echo "========================================"
echo "MOSAIC ETH Production Run - VM Setup"
echo "========================================"
echo "Log file: $LOG_FILE"
echo ""

# Check if running on Ubuntu 20.04
OS_VERSION=$(lsb_release -rs 2>/dev/null || echo "unknown")
echo "Detected OS: Ubuntu $OS_VERSION"

# Verify r-mosaic-Rscript wrapper exists (required for Ubuntu 20.04)
if [ "$OS_VERSION" = "20.04" ]; then
  if [ ! -x "$HOME/bin/r-mosaic-Rscript" ]; then
    echo "ERROR: r-mosaic-Rscript wrapper not found!"
    echo "Please run vm/setup_mosaic_minimal.sh first to set up the environment."
    exit 1
  fi
  R_CMD="$HOME/bin/r-mosaic-Rscript"
  echo "Using r-mosaic-Rscript wrapper for Ubuntu 20.04 compatibility"
else
  R_CMD="Rscript"
  echo "Using standard Rscript"
fi
echo ""

# ==============================================================================
# Step 1: Set up minimal directory structure
# ==============================================================================
echo "[1/4] Setting up directory structure..."

mkdir -p ~/MOSAIC
cd ~/MOSAIC

# Create output directory (ensures log file can be written)
mkdir -p "$OUTPUT_DIR"

echo "  ✓ Directory structure ready"
echo ""

# ==============================================================================
# Step 2: Clone/update required repositories
# ==============================================================================
echo "[2/4] Ensuring repositories are available..."

# Clone or update MOSAIC-pkg
if [ ! -d "MOSAIC-pkg" ]; then
  echo "  Cloning MOSAIC-pkg..."
  git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg.git
else
  echo "  Updating MOSAIC-pkg..."
  cd MOSAIC-pkg
  git pull origin main
  cd ..
fi

# Clone or update MOSAIC-data (needed for location-specific priors/configs)
if [ ! -d "MOSAIC-data" ]; then
  echo "  Cloning MOSAIC-data..."
  git clone https://github.com/InstituteforDiseaseModeling/MOSAIC-data.git
else
  echo "  Updating MOSAIC-data..."
  cd MOSAIC-data
  git pull origin main
  cd ..
fi

echo "  ✓ Repositories ready"
echo ""

# ==============================================================================
# Step 3: Verify R package installation
# ==============================================================================
echo "[3/4] Verifying MOSAIC R package..."

$R_CMD -e "
  .libPaths(c('~/R/library', .libPaths()))
  if (!requireNamespace('MOSAIC', quietly = TRUE)) {
    cat('ERROR: MOSAIC package not installed!\n')
    cat('Run: remotes::install_github(\"InstituteforDiseaseModeling/MOSAIC-pkg\")\n')
    quit(status = 1)
  }
  cat('  ✓ MOSAIC version:', as.character(packageVersion('MOSAIC')), '\n')
"

if [ $? -ne 0 ]; then
  echo "ERROR: MOSAIC package verification failed"
  exit 1
fi

echo ""

# ==============================================================================
# Step 4: Launch MOSAIC ETH run
# ==============================================================================
echo "[4/4] Launching MOSAIC ETH calibration..."
echo ""

# Check if user wants tmux session
USE_TMUX=false
if command -v tmux &> /dev/null; then
  read -p "Run in tmux session for persistence? (recommended) [Y/n]: " -n 1 -r
  echo
  if [[ ! $REPLY =~ ^[Nn]$ ]]; then
    USE_TMUX=true
  fi
fi

# Prepare R script wrapper that sets library path
WRAPPER_SCRIPT=$(cat <<'EOF'
# Set library path for user installation
.libPaths(c('~/R/library', .libPaths()))

# Source the main ETH run script
source('~/MOSAIC/MOSAIC-pkg/vm/run_mosaic_ETH.R')
EOF
)

# Create temporary wrapper script
TEMP_WRAPPER="/tmp/run_mosaic_ETH_wrapper_${TIMESTAMP}.R"
echo "$WRAPPER_SCRIPT" > "$TEMP_WRAPPER"

# Launch command
RUN_CMD="$R_CMD $TEMP_WRAPPER 2>&1 | tee $LOG_FILE"

if [ "$USE_TMUX" = true ]; then
  # Check if tmux session already exists
  if tmux has-session -t "$TMUX_SESSION" 2>/dev/null; then
    echo "WARNING: tmux session '$TMUX_SESSION' already exists!"
    read -p "Attach to existing session? [y/N]: " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
      tmux attach-session -t "$TMUX_SESSION"
      exit 0
    else
      echo "ERROR: Please kill or rename existing session first"
      exit 1
    fi
  fi

  # Create new tmux session
  echo "Starting tmux session '$TMUX_SESSION'..."
  echo ""
  echo "========================================"
  echo "TMUX Commands:"
  echo "  Detach: Ctrl+b then d"
  echo "  Reattach: tmux attach -t $TMUX_SESSION"
  echo "  Kill: tmux kill-session -t $TMUX_SESSION"
  echo "========================================"
  echo ""
  echo "Press Enter to start (tmux will launch in 3 seconds)..."
  read
  sleep 3

  tmux new-session -s "$TMUX_SESSION" "$RUN_CMD"

else
  # Run directly (foreground)
  echo "Running MOSAIC ETH calibration (foreground)..."
  echo "Output will be logged to: $LOG_FILE"
  echo ""
  eval "$RUN_CMD"
fi

# Cleanup temporary wrapper
rm -f "$TEMP_WRAPPER"

echo ""
echo "========================================"
echo "MOSAIC ETH Run Complete"
echo "========================================"
echo "Log file: $LOG_FILE"
echo "Output directory: $OUTPUT_DIR"
echo ""
