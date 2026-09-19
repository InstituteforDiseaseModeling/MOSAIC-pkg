#!/usr/bin/env bash
# Sequential arm queue. Bounded waits only (PROTOCOL.md sec 6).
set -u
cd ~ || exit 1
CORES=${CORES:-40}
NSIMS=${NSIMS:-10000}
TCUT=${TCUT:-2025-09-01}

# bounded wait for the per-arm library installs
for i in $(seq 1 60); do
  grep -q LIBS_DONE ~/inflab_libs.log 2>/dev/null && break
  sleep 20
done
if ! grep -q LIBS_DONE ~/inflab_libs.log 2>/dev/null; then
  echo "[queue] ABORT: libs not ready after 20 min"; exit 1
fi
echo "[queue] libs ready; starting $(date -Is)"

run_one () {  # arm lib seed
  local arm=$1 lib=$2 seed=$3
  local tag="${arm}_s${seed}"
  if [ -f ~/inflab/.done_${tag} ]; then echo "[queue] skip ${tag} (done)"; return; fi
  echo "[queue] START ${tag} $(date -Is)"
  ARM_ID="$arm" LIB="$lib" SEED="$seed" N_SIMS="$NSIMS" CORES="$CORES" T_CUT="$TCUT" ISO=ETH \
    ~/bin/r-mosaic-Rscript ~/inflab_arm.R >~/inflab/log_${tag}.log 2>&1
  local rc=$?
  if [ $rc -eq 0 ]; then touch ~/inflab/.done_${tag}; echo "[queue] OK ${tag} $(date -Is)"
  else echo "[queue] FAIL ${tag} rc=$rc $(date -Is)"; fi
}

for s in 1 2 3; do run_one baseline "~/R/lib_base" $s; done
for s in 1 2 3; do run_one A1       "~/R/lib_A1"   $s; done
echo "[queue] ALL DONE $(date -Is)"
