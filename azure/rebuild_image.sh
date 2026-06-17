#!/usr/bin/env bash
#
# rebuild_image.sh — rebuild & publish the MOSAIC Coiled worker image.
#
# Automates Steps 1–5 + 7 of azure/DOCKER_IMAGE_UPDATE.md:
#   backup current :latest  ->  build (amd64)  ->  tag :latest + :v<version>
#   ->  push both  ->  smoke test.
# The Coiled software-env refresh (Step 6) is a separate concern — run
# azure/refresh_coiled_env.py afterwards.
#
# Safe-by-default: derives the version from DESCRIPTION, always lands a backup
# tag before building, builds for linux/amd64 (Coiled workers are x86_64), and
# prompts before pushing to the shared registry.
#
# Side effect: if azure/Dockerfile's `LABEL version=` differs from DESCRIPTION it
# is rewritten in place to match (git-tracked file left modified — commit it).
#
# Usage:
#   azure/rebuild_image.sh [options]
#     --ref <git-ref>   MOSAIC ref to install in the image (default: main)
#     --no-backup       skip the backup-tag step (NOT recommended)
#     --skip-smoke      skip the post-push smoke test
#     --yes             do not prompt before pushing (for unattended runs)
#     -h, --help        show this help
#
set -euo pipefail

# ---- locate repo root (this script lives in <repo>/azure) -------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

# ---- constants (canonical identifiers; see azure/ACR_SETUP.md) --------------
ACR_NAME="idmmosaicacr"
REGISTRY="idmmosaicacr.azurecr.io"
IMAGE="mosaic-worker"
SUBSCRIPTION="IDM Research 2"
DOCKERFILE="azure/Dockerfile"
PLATFORM="linux/amd64"

# ---- options ---------------------------------------------------------------
MOSAIC_REF="main"
DO_BACKUP=1
DO_SMOKE=1
ASSUME_YES=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --ref)        MOSAIC_REF="$2"; shift 2 ;;
    --no-backup)  DO_BACKUP=0; shift ;;
    --skip-smoke) DO_SMOKE=0; shift ;;
    --yes)        ASSUME_YES=1; shift ;;
    -h|--help)    awk 'NR>1 && /^#/{sub(/^# ?/,"");print;next} NR>1{exit}' "${BASH_SOURCE[0]}"; exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done

say()  { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
die()  { printf '\033[31mERROR: %s\033[0m\n' "$*" >&2; exit 1; }

# ---- version (source of truth) ---------------------------------------------
VERSION="$(grep '^Version:' "$REPO_ROOT/DESCRIPTION" | awk '{print $2}')"
[[ -n "$VERSION" ]] || die "could not read Version: from DESCRIPTION"
say "Package version (from DESCRIPTION): $VERSION"

# ---- prerequisites ---------------------------------------------------------
say "Checking prerequisites"
command -v docker >/dev/null || die "docker not found"
command -v az     >/dev/null || die "az (Azure CLI) not found"
docker info >/dev/null 2>&1  || die "Docker daemon not running"

CUR_SUB="$(az account show --query name -o tsv 2>/dev/null || true)"
if [[ "$CUR_SUB" != "$SUBSCRIPTION" ]]; then
  echo "Azure subscription is '${CUR_SUB:-<none>}', expected '$SUBSCRIPTION'."
  echo "Run: az login && az account set --subscription \"$SUBSCRIPTION\""
  die "wrong/unset Azure subscription"
fi
say "Logging in to ACR ($ACR_NAME)"
az acr login --name "$ACR_NAME" >/dev/null

# ---- keep Dockerfile LABEL in sync (metadata only) -------------------------
LABEL_LINE="$(grep -n '^LABEL version=' "$REPO_ROOT/$DOCKERFILE" | head -1 || true)"
if [[ -n "$LABEL_LINE" ]] && ! grep -q "^LABEL version=\"$VERSION\"" "$REPO_ROOT/$DOCKERFILE"; then
  say "Updating Dockerfile LABEL version= to $VERSION (was: ${LABEL_LINE#*:})"
  # perl used for a portable in-place edit (BSD/macOS vs GNU sed differ on -i)
  perl -0pi -e "s/^LABEL version=\"[^\"]*\"/LABEL version=\"$VERSION\"/m" "$REPO_ROOT/$DOCKERFILE"
fi

# ---- Step 1: backup current :latest ----------------------------------------
if [[ "$DO_BACKUP" -eq 1 ]]; then
  say "Backing up current :latest before building"
  if docker pull "$REGISTRY/$IMAGE:latest" >/dev/null 2>&1; then
    OLD_LABEL="$(docker inspect --format '{{ index .Config.Labels "version" }}' \
                   "$REGISTRY/$IMAGE:latest" 2>/dev/null || echo unknown)"
    [[ -n "$OLD_LABEL" ]] || OLD_LABEL="unknown"  # image present but no version label
    # Include time so a second rebuild on the same day can't overwrite the first
    # day's rollback anchor (the runbook calls the backup a permanent anchor).
    BACKUP_TAG="backup-$(date +%F-%H%M%S)-v${OLD_LABEL}"
    docker tag  "$REGISTRY/$IMAGE:latest" "$REGISTRY/$IMAGE:$BACKUP_TAG"
    docker push "$REGISTRY/$IMAGE:$BACKUP_TAG"
    say "Backup pushed: $REGISTRY/$IMAGE:$BACKUP_TAG (rollback anchor)"
  else
    echo "No existing :latest to back up (first publish?) — continuing."
  fi
else
  echo "Skipping backup (--no-backup)."
fi

# ---- Step 3: build (amd64) -------------------------------------------------
say "Building $IMAGE:latest for $PLATFORM (ref=$MOSAIC_REF) — ~15-25 min"
( cd "$REPO_ROOT" && docker build --platform "$PLATFORM" \
    -f "$DOCKERFILE" --build-arg "MOSAIC_REF=$MOSAIC_REF" \
    -t "$IMAGE:latest" . )

# ---- Step 4: tag for ACR ---------------------------------------------------
say "Tagging for ACR"
docker tag "$IMAGE:latest" "$REGISTRY/$IMAGE:latest"
docker tag "$IMAGE:latest" "$REGISTRY/$IMAGE:v$VERSION"

# ---- Step 5: push (confirm first) ------------------------------------------
if [[ "$ASSUME_YES" -ne 1 ]]; then
  echo
  echo "About to push to the SHARED registry $REGISTRY:"
  echo "    $IMAGE:latest   and   $IMAGE:v$VERSION"
  read -r -p "Proceed with push? [y/N] " ans
  [[ "$ans" == "y" || "$ans" == "Y" ]] || die "aborted before push"
fi
say "Pushing :latest and :v$VERSION"
docker push "$REGISTRY/$IMAGE:latest"
docker push "$REGISTRY/$IMAGE:v$VERSION"

# ---- Step 7: smoke test ----------------------------------------------------
# Runs AFTER the push (it tests the published image), so a failure here does NOT
# un-publish anything. On an arm64 host the amd64 image runs under qemu emulation
# (Docker Desktop / Rosetta); if emulation is unavailable this fails with
# "exec format error" even though the image is fine — hence non-fatal + a hint.
if [[ "$DO_SMOKE" -eq 1 ]]; then
  say "Smoke-testing the published image (amd64; needs qemu emulation on arm64 hosts)"
  if ! docker run --rm --platform "$PLATFORM" "$REGISTRY/$IMAGE:latest" \
        R -e "library(MOSAIC); MOSAIC::check_dependencies(); cat('OK\n')"; then
    echo "WARNING: smoke test failed. The image is already PUSHED. If you are on" >&2
    echo "  Apple Silicon this may just be missing amd64 emulation — re-test on an" >&2
    echo "  x86_64 host (e.g. hedgehog) before trusting it. Otherwise investigate." >&2
  fi
fi

say "Done. Published $REGISTRY/$IMAGE:latest (= :v$VERSION)."
echo "Next: refresh the Coiled env  ->  python azure/refresh_coiled_env.py"
echo "Rollback anchor: ${BACKUP_TAG:-<none created>}"
