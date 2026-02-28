#!/usr/bin/env python
"""
Simple MOSAIC Coiled Runner
Calls existing run_MOSAIC() R function on workers (no reimplementation needed!)

Usage:
    python run_mosaic_simple.py --iso ETH --output-dir ./test-output

Author: MOSAIC Team
"""

import sys
import argparse
import subprocess

def main():
    parser = argparse.ArgumentParser(description='Run MOSAIC on Coiled (simple)')
    parser.add_argument('--iso', required=True, help='Comma-separated ISO codes')
    parser.add_argument('--output-dir', default='./output')
    parser.add_argument('--coiled-env', default='mosaic-docker-workers')

    args = parser.parse_args()

    # Just call run_MOSAIC directly via R
    r_script = f"""
    library(MOSAIC)

    iso_codes <- c("{args.iso}")
    config <- get_location_config(iso=iso_codes)
    priors <- get_location_priors(iso=iso_codes)
    control <- mosaic_control_defaults()

    control$calibration$n_simulations <- 100
    control$calibration$n_iterations <- 2

    result <- run_MOSAIC(
        dir_output = "{args.output_dir}",
        config = config,
        priors = priors,
        control = control
    )

    cat("✅ MOSAIC completed!\\n")
    """

    print("Running MOSAIC (direct R call)...")
    result = subprocess.run(['Rscript', '-e', r_script], timeout=3600)

    return result.returncode

if __name__ == "__main__":
    sys.exit(main())
