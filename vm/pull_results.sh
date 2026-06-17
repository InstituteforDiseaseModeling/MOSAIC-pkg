#!/usr/bin/env bash
# Pull MOSAIC calibration results (*.tar.gz) from the hedgehog Azure VM to a local dir.
#
# Usage:
#   vm/pull_results.sh                       # pull ALL *.tar.gz
#   vm/pull_results.sh ZWE ZMB MOZ           # pull only these ISO3 archives
#   vm/pull_results.sh --list                # list remote archives, download nothing
#   vm/pull_results.sh --dest ~/results ETH  # custom local destination
#
# Env overrides:
#   HEDGEHOG_HOST        ssh target            (default: hedgehog  -- the ~/.ssh/config alias)
#   HEDGEHOG_REMOTE_DIR  remote results dir    (default: MOSAIC/output/individual)
#   HEDGEHOG_DEST        local destination dir (default: ./output/individual)
set -euo pipefail

HOST="${HEDGEHOG_HOST:-hedgehog}"
REMOTE_DIR="${HEDGEHOG_REMOTE_DIR:-MOSAIC/output/individual}"
DEST="${HEDGEHOG_DEST:-./output/individual}"
LIST_ONLY=0
ISO_CODES=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --list)  LIST_ONLY=1; shift ;;
    --dest)  DEST="$2"; shift 2 ;;
    -h|--help)
      awk 'NR>1 && /^#/ {sub(/^# ?/,""); print; next} NR>1 {exit}' "$0"; exit 0 ;;
    -*)
      echo "Unknown option: $1" >&2; exit 2 ;;
    *)
      ISO_CODES+=("$1"); shift ;;
  esac
done

echo "======================================"
echo "MOSAIC results pull from '$HOST'"
echo "  remote: $REMOTE_DIR"
echo "  local : $DEST"
echo "======================================"

# Verify connectivity (key auth, non-interactive) before doing anything.
if ! ssh -o BatchMode=yes -o ConnectTimeout=15 "$HOST" true 2>/dev/null; then
  echo "ERROR: cannot reach '$HOST' non-interactively. Is the VM running and is key auth set up?" >&2
  echo "  Try: ssh $HOST     (and 'az vm start' if it is deallocated)" >&2
  exit 1
fi

if [[ "$LIST_ONLY" -eq 1 ]]; then
  echo "Remote archives:"
  ssh "$HOST" "ls -lh ${REMOTE_DIR}/*.tar.gz 2>/dev/null" || echo "  (none found)"
  exit 0
fi

mkdir -p "$DEST"

if [[ ${#ISO_CODES[@]} -eq 0 ]]; then
  echo "Pulling ALL *.tar.gz ..."
  scp "${HOST}:${REMOTE_DIR}/*.tar.gz" "$DEST"/
else
  for iso in "${ISO_CODES[@]}"; do
    echo "Pulling ${iso}.tar.gz ..."
    scp "${HOST}:${REMOTE_DIR}/${iso}.tar.gz" "$DEST"/
  done
fi

echo "Done. Files in: $DEST"
ls -lh "$DEST"/*.tar.gz 2>/dev/null || true
