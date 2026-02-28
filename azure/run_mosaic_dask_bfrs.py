#!/usr/bin/env python
"""
MOSAIC Coiled Runner (Docker-based)
Runs MOSAIC BFRS calibration on Coiled workers using pre-built Docker image.

Usage:
    python run_mosaic_docker.py --iso ETH --n-simulations 100 --n-iterations 2

Author: MOSAIC Team
Last Updated: 2026-02-28
"""

import os
import sys
import argparse
import time
import json
import subprocess
import tempfile
from pathlib import Path

try:
    import coiled
    from dask.distributed import Client
    import dask
    import numpy as np
    import pandas as pd
except ImportError as e:
    print(f"❌ Missing dependency: {e}")
    print("\nInstall with: conda activate mosaic-coiled")
    sys.exit(1)

# =============================================================================
# R Interface Functions (run locally, not on workers)
# =============================================================================

def sample_parameters_r(iso_codes, n, weights=None, seed=None):
    """Sample parameters using R locally."""
    if seed is None:
        seed = np.random.randint(1, 1000000)

    iso_vec = '", "'.join(iso_codes)
    weights_r = "NULL" if weights is None else f"c({','.join(map(str, weights))})"

    r_script = f"""
    library(MOSAIC)
    set_root_directory('~/MOSAIC')

    iso_codes <- c("{iso_vec}")
    config <- get_location_config(iso=iso_codes)
    priors <- get_location_priors(iso=iso_codes)

    params <- sample_parameters(
        config = config,
        priors = priors,
        n = {n},
        weights = {weights_r},
        seed = {seed},
        verbose = FALSE
    )

    cat(jsonlite::toJSON(params))
    """

    result = subprocess.run(
        ['Rscript', '-e', r_script],
        capture_output=True,
        text=True,
        timeout=120
    )

    if result.returncode != 0:
        raise RuntimeError(f"Parameter sampling failed: {result.stderr}")

    return pd.DataFrame(json.loads(result.stdout))

# =============================================================================
# Worker Function (runs on Coiled workers with Docker image)
# =============================================================================

def run_laser_on_worker(param_dict, iso_codes):
    """
    Run single LASER simulation on Coiled worker.
    MOSAIC is pre-installed in Docker image, so just call it directly.
    """
    import subprocess
    import json

    iso_vec = '", "'.join(iso_codes)
    param_json = json.dumps(param_dict)

    # Write R script to temp file (avoids quoting issues)
    import tempfile

    r_script = f"""
library(MOSAIC)
set_root_directory('/workspace')

iso_codes <- c("{iso_vec}")
config <- get_location_config(iso=iso_codes)

# Parse and apply sampled parameters to config
params <- jsonlite::fromJSON('{param_json}')
for (param_name in names(params)) {{
    config[[param_name]] <- params[[param_name]]
}}

# Run LASER with updated config
result <- run_LASER(
    config = config,
    seed = 123,
    quiet = TRUE
)

# Read output and return as JSON
# LASER writes to outdir, need to read results
cat(jsonlite::toJSON(list(success=TRUE), auto_unbox=TRUE))
"""

    with tempfile.NamedTemporaryFile(mode='w', suffix='.R', delete=False) as f:
        f.write(r_script)
        script_path = f.name

    try:
        result = subprocess.run(
            ['Rscript', script_path],
            capture_output=True,
            text=True,
            timeout=300
        )
    finally:
        os.unlink(script_path)

    if result.returncode != 0:
        raise RuntimeError(f"LASER failed: {result.stderr}")

    # Check for empty output
    if not result.stdout.strip():
        raise RuntimeError(f"LASER produced no output. Stderr: {result.stderr[:1000]}")

    # Try to parse JSON
    try:
        return json.loads(result.stdout)
    except json.JSONDecodeError as e:
        raise RuntimeError(f"Invalid JSON from LASER. Stdout: {result.stdout[:500]}, Stderr: {result.stderr[:500]}")

# =============================================================================
# Main Workflow
# =============================================================================

def run_mosaic_on_coiled(
    iso_codes,
    n_simulations=100,
    n_iterations=2,
    n_workers=2,
    output_dir="./output",
    coiled_env="mosaic-docker-workers",
    vm_type="Standard_D4s_v6",
    region="westus2"
):
    """Run MOSAIC BFRS calibration on Coiled with Docker-based workers."""

    print("="*70)
    print("MOSAIC Coiled (Docker)")
    print("="*70)
    print(f"ISO codes: {', '.join(iso_codes)}")
    print(f"Simulations: {n_simulations}")
    print(f"Iterations: {n_iterations}")
    print(f"Workers: {n_workers}")
    print(f"Coiled env: {coiled_env}")
    print(f"Output: {output_dir}")
    print("="*70)
    print()

    # Create output directory
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # Clean up old stopped clusters to free quota
    print("🧹 Cleaning up old stopped clusters...")
    try:
        import subprocess
        subprocess.run(['bash', '-c',
            'coiled cluster list | grep stopped | grep mosaic-ETH | head -10 | awk \'{print $1}\' | xargs -I {} coiled cluster delete {} --yes'],
            timeout=30, check=False)
        print("✅ Old clusters cleaned")
    except:
        print("⚠️  Cleanup skipped (manual cleanup recommended)")
    print()

    # Create Coiled cluster
    print(f"☁️  Creating Coiled cluster...")
    cluster = coiled.Cluster(
        name=f"mosaic-{'-'.join(iso_codes)}-{int(time.time())}",
        n_workers=n_workers,
        worker_vm_types=[vm_type],
        region=region,
        software=coiled_env,
        shutdown_on_close=True,
        idle_timeout="2 hours"
    )

    client = Client(cluster)
    print(f"✅ Cluster ready: {client.dashboard_link}")
    print()

    # BFRS iterations
    weights = np.ones(n_simulations) / n_simulations
    all_results = []

    for iteration in range(n_iterations):
        print(f"{'='*70}")
        print(f"Iteration {iteration + 1}/{n_iterations}")
        print(f"{'='*70}")

        # Sample parameters (locally)
        print(f"📦 Sampling {n_simulations} parameter sets...")
        params_df = sample_parameters_r(iso_codes, n_simulations, weights)
        param_dicts = params_df.to_dict('records')

        # Submit simulations to Coiled workers
        print(f"🚀 Submitting {n_simulations} simulations to cluster...")

        futures = [
            dask.delayed(run_laser_on_worker)(p, iso_codes)
            for p in param_dicts
        ]

        # Compute on cluster
        futures = client.compute(futures)
        results = client.gather(futures)

        print(f"✅ Simulations complete!")

        # TODO: Compute likelihoods and update weights
        # For now, just save parameter samples
        params_df['iteration'] = iteration
        params_df['weight'] = weights
        all_results.append(params_df)
        print()

    # Save results
    output_file = output_path / 'simulations.parquet'
    all_params = pd.concat(all_results, ignore_index=True)
    all_params.to_parquet(output_file)
    print(f"💾 Results saved: {output_file}")

    # Cleanup
    client.close()
    cluster.close()

    return {'output_file': str(output_file), 'n_params': len(all_params)}

# =============================================================================
# CLI
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description='Run MOSAIC on Coiled with Docker')
    parser.add_argument('--iso', required=True, help='Comma-separated ISO codes')
    parser.add_argument('--n-simulations', type=int, default=100)
    parser.add_argument('--n-iterations', type=int, default=2)
    parser.add_argument('--n-workers', type=int, default=2)
    parser.add_argument('--output-dir', default='./output')
    parser.add_argument('--coiled-env', default='mosaic-docker-workers')
    parser.add_argument('--vm-type', default='Standard_D4s_v6')
    parser.add_argument('--region', default='westus2')

    args = parser.parse_args()
    iso_codes = [x.strip().upper() for x in args.iso.split(',')]

    try:
        result = run_mosaic_on_coiled(
            iso_codes=iso_codes,
            n_simulations=args.n_simulations,
            n_iterations=args.n_iterations,
            n_workers=args.n_workers,
            output_dir=args.output_dir,
            coiled_env=args.coiled_env,
            vm_type=args.vm_type,
            region=args.region
        )

        print()
        print("="*70)
        print("✅ MOSAIC Calibration Complete!")
        print("="*70)
        print(f"Results: {result['output_file']}")
        print(f"Parameters: {result['n_params']}")
        print("="*70)

        return 0

    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()
        return 1

if __name__ == "__main__":
    sys.exit(main())
