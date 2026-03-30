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
    import tempfile
    import os

    # Create R script as temp file (avoids quoting hell with -e)
    r_script = f"""
library(MOSAIC)

# Attach Python environment (CRITICAL for LASER simulations!)
cat("Attaching MOSAIC Python environment...\\n")
MOSAIC::use_mosaic_env()
cat("✅ Python environment attached\\n")

# Data is included in Docker image - verify it exists
cat("Verifying MOSAIC data directories...\\n")
if (!dir.exists('/workspace/MOSAIC/MOSAIC-data')) {{
    cat("❌ ERROR: MOSAIC-data not found in Docker image\\n")
    system('ls -la /workspace/MOSAIC/')
    quit(status = 1)
}}
if (!dir.exists('/workspace/MOSAIC/MOSAIC-pkg')) {{
    cat("❌ ERROR: MOSAIC-pkg not found in Docker image\\n")
    system('ls -la /workspace/MOSAIC/')
    quit(status = 1)
}}
cat("✅ MOSAIC data directories verified\\n")

# Set MOSAIC root directory (data is already there!)
set_root_directory('/workspace/MOSAIC')

# Configure MOSAIC for this country
iso_codes <- c("{iso_code}")

cat(strrep("=", 70), "\\n")
cat("MOSAIC Calibration: {iso_code}\\n")
cat(strrep("=", 70), "\\n")
cat("Step 1: Getting location config...\\n")
config <- get_location_config(iso=iso_codes)
cat("  ✓ Config loaded for:", config$location_name, "\\n")

cat("Step 2: Getting location priors...\\n")
priors <- get_location_priors(iso=iso_codes)
cat("  ✓ Priors loaded\\n")

cat("Step 3: Setting up control parameters...\\n")
control <- mosaic_control_defaults()
control$calibration$n_simulations <- {n_simulations}
control$calibration$n_iterations <- {n_iterations}
control$parallel$enable <- TRUE
control$parallel$n_cores <- max(1, parallel::detectCores() - 1)
cat("  ✓ Simulations:", {n_simulations}, "\\n")
cat("  ✓ Iterations:", {n_iterations}, "\\n")
cat("  ✓ Cores:", control$parallel$n_cores, "\\n")

cat("Step 4: Creating output directory...\\n")
dir_output <- file.path('/workspace', 'output', '{iso_code}')
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)
cat("  ✓ Output dir:", dir_output, "\\n")

cat(strrep("=", 70), "\\n")
cat("Starting MOSAIC calibration...\\n")
cat(strrep("=", 70), "\\n")

# Run MOSAIC (existing proven workflow!)
tryCatch({{
    result <- run_MOSAIC(
        dir_output = dir_output,
        config = config,
        priors = priors,
        control = control
    )
    cat("\\n")
    cat(strrep("=", 70), "\\n")
    cat("✅ MOSAIC calibration complete for {iso_code}!\\n")
    cat(strrep("=", 70), "\\n")
}}, error = function(e) {{
    cat("\\n")
    cat(strrep("=", 70), "\\n")
    cat("❌ ERROR in MOSAIC calibration:\\n")
    cat(strrep("=", 70), "\\n")
    cat("Error message:\\n")
    cat(as.character(e$message), "\\n")
    cat("\\n")
    cat("Traceback:\\n")
    print(sys.calls())
    quit(status = 1)
}})
"""

    print(f"[{iso_code}] Running MOSAIC calibration...")

    # Write R script to temp file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.R', delete=False) as f:
        f.write(r_script)
        script_path = f.name

    try:
        # Run R script from file (much cleaner than -e!)
        result = subprocess.run(
            ['Rscript', script_path],
            capture_output=True,
            text=True,
            timeout=7200  # 2 hour timeout per country
        )

        # Print stdout for visibility
        if result.stdout:
            print(f"[{iso_code}] Output:")
            for line in result.stdout.split('\n'):
                if line.strip():
                    print(f"  {line}")

        if result.returncode == 0:
            print(f"[{iso_code}] ✅ Complete!")
            return {'country': iso_code, 'status': 'success', 'output': result.stdout}
        else:
            print(f"[{iso_code}] ❌ Failed (exit code {result.returncode})")
            if result.stderr:
                print(f"[{iso_code}] Error output:")
                for line in result.stderr.split('\n')[:50]:  # Limit error output
                    if line.strip():
                        print(f"  {line}")
            return {
                'country': iso_code,
                'status': 'failed',
                'error': result.stderr,
                'output': result.stdout,
                'exit_code': result.returncode
            }
    finally:
        # Cleanup temp file
        try:
            os.unlink(script_path)
        except:
            pass

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
    print(f"Data source: Included in Docker image")
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
        if result['status'] == 'failed':
            if 'output' in result and result['output']:
                print(f"   Stdout (last 3000 chars): {result['output'][-3000:]}")
            if 'error' in result and result['error']:
                print(f"   Stderr (last 3000 chars): {result['error'][-3000:]}")

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
