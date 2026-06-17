#!/usr/bin/env python3
"""refresh_coiled_env.py — refresh the MOSAIC Coiled software environment.

Step 6 of azure/DOCKER_IMAGE_UPDATE.md. Deletes and recreates the Coiled
software environment so it picks up a freshly pushed image. We delete+recreate
rather than rely on force_rebuild because the tag name (:latest) is unchanged,
and Coiled often keeps the old cached image digest in that case.

Safe to run as long as no calibration is actively queuing workers; already
running clusters are unaffected (they already hold the image).

Usage:
    python azure/refresh_coiled_env.py [--yes]
        --name        Coiled software env  (default: mosaic-acr-workers)
        --workspace   Coiled workspace     (default: idm-coiled-idmad-r2)
        --container   image ref            (default: idmmosaicacr.azurecr.io/mosaic-worker:latest)
        --yes         do not prompt for confirmation
"""
import argparse
import sys

DEFAULT_NAME = "mosaic-acr-workers"
DEFAULT_WORKSPACE = "idm-coiled-idmad-r2"
DEFAULT_CONTAINER = "idmmosaicacr.azurecr.io/mosaic-worker:latest"


def main() -> int:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--name", default=DEFAULT_NAME)
    p.add_argument("--workspace", default=DEFAULT_WORKSPACE)
    p.add_argument("--container", default=DEFAULT_CONTAINER)
    p.add_argument("--yes", action="store_true",
                   help="skip the confirmation prompt")
    args = p.parse_args()

    try:
        import coiled
    except ImportError:
        print("ERROR: the 'coiled' package is not importable in this Python "
              "environment. Activate the env that has coiled and is logged in "
              "to the workspace, then retry.", file=sys.stderr)
        return 1

    print(f"Coiled software env : {args.name}")
    print(f"Workspace           : {args.workspace}")
    print(f"Container           : {args.container}")
    print("This will DELETE then RECREATE the environment above.")
    print("Only run when no calibration is actively queuing workers "
          "(running clusters are unaffected).")

    if not args.yes:
        ans = input("Proceed? [y/N] ").strip().lower()
        if ans != "y":
            print("Aborted.")
            return 1

    print("Deleting old env...")
    coiled.delete_software_environment(name=args.name, workspace=args.workspace)

    # Partial-failure guard: the env is now DELETED. If recreate throws (auth
    # expired, wrong workspace, bad container ref, transient API error) the env
    # would be left missing and any calibration referencing it would fail to
    # start workers. Surface a loud, explicit recovery instruction.
    print("Recreating env...")
    try:
        coiled.create_software_environment(
            name=args.name,
            container=args.container,
            workspace=args.workspace,
        )
    except Exception as exc:  # noqa: BLE001 - we re-raise after the recovery notice
        print(
            f"\n*** RECREATE FAILED: {exc}\n"
            f"*** The Coiled env '{args.name}' is now DELETED and was NOT recreated.\n"
            f"*** Fix the cause (e.g. `coiled login`, check workspace/container),\n"
            f"*** then re-run this script to recreate it before launching any job.",
            file=sys.stderr,
        )
        return 1

    print("Done. New workers will pull the refreshed image.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
