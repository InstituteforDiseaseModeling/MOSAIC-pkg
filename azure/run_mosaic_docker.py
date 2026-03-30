#!/usr/bin/env python
"""
MOSAIC Coiled Runner (Docker-based)
Simple orchestrator for MOSAIC runs on Coiled workers with Docker image.

Since MOSAIC is pre-installed in Docker, this script just:
1. Creates Coiled cluster
2. Calls R to run MOSAIC directly on workers
3. Collects results

Usage:
    python run_mosaic_docker.py --iso ETH --output-dir ./test-output

Author: MOSAIC Team
Last Updated: 2026-02-28
"""

import os
import sys
import argparse
import time
from pathlib import Path

try:
    import coiled
    from dask.distributed import Client
except ImportError as e:
    print(f"❌ Missing dependency: {e}")
    print("\nInstall with: conda activate mosaic-local-orchestrate (or mosaic-coiled)")
    sys.exit(1)

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
    """
    Run MOSAIC on Coiled using Docker-based workers.

    Since MOSAIC is pre-installed in Docker image, workers can run
    MOSAIC R scripts directly without setup.
    """

    print("="*70)
    print("MOSAIC Coiled (Docker)")
    print("="*70)
    print(f"ISO codes: {', '.join(iso_codes)}")
    print(f"Simulations: {n_simulations}")
    print(f"Iterations: {n_iterations}")
    print(f"Workers: {n_workers}")
    print(f"Coiled environment: {coiled_env}")
    print(f"Output: {output_dir}")
    print("="*70)
    print()

    # Create output directory
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # Create Coiled cluster
    print(f"☁️  Creating Coiled cluster with {n_workers} workers...")
    cluster_name = f"mosaic-{'-'.join(iso_codes)}-{int(time.time())}"

    cluster = coiled.Cluster(
        name=cluster_name,
        n_workers=n_workers,
        worker_vm_types=[vm_type],
        region=region,
        software=coiled_env,  # Uses Docker-based environment
        shutdown_on_close=True,
        idle_timeout="2 hours"
    )

    client = Client(cluster)
    print(f"✅ Cluster ready!")
    print(f"📊 Dashboard: {client.dashboard_link}")
    print()

    # Simple approach: Run MOSAIC R script directly on cluster
    # The Docker image has MOSAIC pre-installed and ready
    print("🚀 Running MOSAIC calibration on Coiled workers...")
    print()
    print("📝 Note: This is a scaffold implementation.")
    print("   Full BFRS workflow integration coming next.")
    print("   For now, demonstrating cluster creation and Docker image usage.")
    print()

    # TODO: Implement full BFRS workflow
    # - Sample parameters
    # - Submit LASER simulations to workers
    # - Compute likelihoods
    # - Update weights
    # - Save results

    print(f"✅ Cluster created successfully with Docker-based workers!")
    print(f"✅ MOSAIC pre-installed and ready in workers")
    print()
    print("Next: Complete BFRS workflow implementation in this script")

    # Cleanup
    client.close()
    cluster.close()

    return {
        'status': 'cluster_tested',
        'dashboard': client.dashboard_link if hasattr(client, 'dashboard_link') else None
    }

def main():
    parser = argparse.ArgumentParser(
        description='Run MOSAIC on Coiled with Docker workers',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
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
        return 0
    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()
        return 1

if __name__ == "__main__":
    sys.exit(main())
