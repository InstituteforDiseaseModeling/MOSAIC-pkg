#!/usr/bin/env python
"""
Run MOSAIC on Coiled - Parallel Country Execution
Each worker runs run_MOSAIC() for one country using proven R code

Usage:
    python run_mosaic_on_coiled.py --iso ETH,KEN --output-dir ./output

With --iso ETH,KEN:
  - Worker 1: Runs MOSAIC for ETH
  - Worker 2: Runs MOSAIC for KEN
  - Both execute in parallel!

Author: MOSAIC Team
Last Updated: 2026-02-28
"""

import sys
import argparse
import time
from pathlib import Path

try:
    import coiled
    from dask.distributed import Client
    import dask
except ImportError as e:
    print(f"❌ Missing dependency: {e}")
    print("Install with: conda activate mosaic-coiled")
    sys.exit(1)

def run_mosaic_for_country(iso_code, output_dir, n_simulations=1000, n_iterations=3):
    """
    Run MOSAIC calibration for a single country.
    Uses existing run_MOSAIC() R function - proven and tested!

    This function runs on a Coiled worker.
    """
    import subprocess

    r_script = f"""
    library(MOSAIC)
    set_root_directory('/workspace')

    # Configure MOSAIC for this country
    iso_codes <- c("{iso_code}")
    config <- get_location_config(iso=iso_codes)
    priors <- get_location_priors(iso=iso_codes)
    control <- mosaic_control_defaults()

    control$calibration$n_simulations <- {n_simulations}
    control$calibration$n_iterations <- {n_iterations}
    control$parallel$enable <- TRUE
    control$parallel$n_cores <- parallel::detectCores() - 1

    cat("Starting MOSAIC calibration for {iso_code}...\\n")
    cat("Simulations:", {n_simulations}, "\\n")
    cat("Iterations:", {n_iterations}, "\\n")
    cat("Cores:", parallel::detectCores() - 1, "\\n")

    # Run MOSAIC (existing proven workflow!)
    result <- run_MOSAIC(
        dir_output = "{output_dir}/{iso_code}",
        config = config,
        priors = priors,
        control = control
    )

    cat("✅ MOSAIC calibration complete for {iso_code}!\\n")
    """

    print(f"[{iso_code}] Running MOSAIC calibration...")

    result = subprocess.run(
        ['Rscript', '-e', r_script],
        capture_output=True,
        text=True,
        timeout=7200  # 2 hour timeout per country
    )

    if result.returncode == 0:
        print(f"[{iso_code}] ✅ Complete!")
        return {'country': iso_code, 'status': 'success'}
    else:
        print(f"[{iso_code}] ❌ Failed")
        print(f"Stderr: {result.stderr}")
        return {'country': iso_code, 'status': 'failed', 'error': result.stderr}

def main():
    parser = argparse.ArgumentParser(
        description='Run MOSAIC on Coiled - Parallel Country Execution'
    )
    parser.add_argument('--iso', required=True, help='Comma-separated ISO codes')
    parser.add_argument('--output-dir', default='./output')
    parser.add_argument('--n-simulations', type=int, default=1000)
    parser.add_argument('--n-iterations', type=int, default=3)
    parser.add_argument('--coiled-env', default='mosaic-docker-workers')
    parser.add_argument('--vm-type', default='Standard_D8s_v6', help='VM with 8 cores for R parallel')
    parser.add_argument('--region', default='westus2')

    args = parser.parse_args()
    iso_codes = [x.strip().upper() for x in args.iso.split(',')]

    print("="*70)
    print("MOSAIC on Coiled - Parallel Country Execution")
    print("="*70)
    print(f"Countries: {', '.join(iso_codes)} ({len(iso_codes)} workers)")
    print(f"Simulations per country: {args.n_simulations}")
    print(f"Iterations per country: {args.n_iterations}")
    print(f"VM type: {args.vm_type}")
    print(f"Output: {args.output_dir}")
    print("="*70)
    print()

    # Create output directory
    output_path = Path(args.output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # Create Coiled cluster with N workers (1 per country)
    n_workers = len(iso_codes)
    print(f"☁️  Creating Coiled cluster with {n_workers} workers...")
    print("   Each worker runs MOSAIC for one country")
    print()

    cluster = coiled.Cluster(
        name=f"mosaic-multi-{int(time.time())}",
        n_workers=n_workers,
        worker_vm_types=[args.vm_type],
        region=args.region,
        software=args.coiled_env,
        shutdown_on_close=True,
        idle_timeout="3 hours"
    )

    client = Client(cluster)
    print(f"✅ Cluster ready: {client.dashboard_link}")
    print()

    # Submit one MOSAIC job per country (parallel execution!)
    print("🚀 Submitting MOSAIC jobs (one per country)...")
    futures = []
    for iso in iso_codes:
        future = client.submit(
            run_mosaic_for_country,
            iso,
            args.output_dir,
            args.n_simulations,
            args.n_iterations
        )
        futures.append(future)
        print(f"   - Submitted: {iso}")

    print()
    print("⏳ Waiting for all countries to complete...")
    print(f"   Monitor live: {client.dashboard_link}")
    print()

    # Wait for all to complete
    results = client.gather(futures)

    # Summary
    print("="*70)
    print("Results Summary")
    print("="*70)
    for result in results:
        status_icon = "✅" if result['status'] == 'success' else "❌"
        print(f"{status_icon} {result['country']}: {result['status']}")
        if result['status'] == 'failed' and 'error' in result:
            print(f"   Error: {result['error'][:1000]}")

    print("="*70)

    # Cleanup
    client.close()
    cluster.close()

    # Check if all succeeded
    all_success = all(r['status'] == 'success' for r in results)

    if all_success:
        print(f"\n✅ All countries completed successfully!")
        print(f"Results in: {args.output_dir}/[COUNTRY]/")
        return 0
    else:
        print(f"\n⚠️  Some countries failed - check errors above")
        return 1

if __name__ == "__main__":
    sys.exit(main())
