#!/bin/bash
# Simple Coiled test with data included in Docker image
# No Azure credentials needed!

set -e

cd ~/MOSAIC/MOSAIC-pkg

# Activate conda environment
source ~/miniforge3/etc/profile.d/conda.sh
conda activate mosaic-coiled

echo "========================================================================"
echo "MOSAIC on Coiled - Simple Test (Data in Docker)"
echo "========================================================================"
echo "Test: Single country (ETH), 10 simulations, 1 iteration"
echo "Data: Included in Docker image (no mounting needed!)"
echo "========================================================================"
echo ""

# Run test
python azure/run_mosaic_parallel_country.py \
  --iso ETH \
  --n-simulations 100 \
  --n-iterations 1 \
  --output-dir ./coiled-simple-test \
  --vm-type Standard_D4s_v6

echo ""
echo "========================================================================"
echo "Test complete! Check results in: ./coiled-simple-test/"
echo "========================================================================"
