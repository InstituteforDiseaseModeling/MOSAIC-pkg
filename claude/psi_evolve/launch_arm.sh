#!/usr/bin/env bash
# Launch one arm across N sharded processes on dugong (objective v3).
#
# usage: launch_arm.sh ARM NSHARD NSEEDS [THREADS]
#   ARM      arm id (also the psi cache dir suffix)
#   NSHARD   processes; 9 = one per production cutoff (the natural shard)
#   NSEEDS   ensemble size; 10 matches production/P001. Seed count is a
#            REGISTERED dimension (PROTOCOL 3b) -- do not lower it to save time,
#            use PSI_EPOCH_SELECT (HA-02) instead.
#   THREADS  per-process core budget (176/NSHARD is the cap)
#
# Arm knobs, all read by run_arm.R:
#   PSI_GEOM=p000|p001|F1|F2|F3|F4     inner-CV geometry preset
#   PSI_TRUNK=lstm|gru|tcn             architecture (N arms)
#   PSI_FEATURE_SET=v7.3|v7.4          data (D arms; v7.4 = leak-free panels)
#   PSI_EPOCH_SELECT=k                 HA-02: fold loop on k seeds, refit all
#   PSI_SEED_BASE=1001                 REPLICATE (disjoint seed block -> floor)
#   PSI_LEAD=12                        lead-h target
#   PSI_EXCLUDE=a,b,c                  feature restriction
#
# examples:
#   ./launch_arm.sh P000 9 10 18                                 # the incumbent refit
#   PSI_SEED_BASE=1001 ./launch_arm.sh P000R 9 10 18             # its replicate -> FLOOR
#   PSI_GEOM=F1 PSI_EPOCH_SELECT=2 ./launch_arm.sh F1 9 10 18    # 677-fold inner CV
#   PSI_TRUNK=tcn PSI_GEOM=F4 PSI_EPOCH_SELECT=2 ./launch_arm.sh N1 9 10 18
set -u
ARM=${1:-P000}; N=${2:-9}; SEEDS=${3:-10}; THREADS=${4:-18}
cd "$HOME/psi_evolve" || exit 1

# PROTOCOL section 6 kill switch: agents check for STOP before any launch.
if [ -f STOP ]; then echo "STOP file present -- refusing to launch (PROTOCOL 6)."; exit 1; fi

# Concurrency cap is hard: MOSAIC_PSI_CORE_BUDGET = floor(176 / n_processes).
CAP=$(( 176 / N ))
if [ "$THREADS" -gt "$CAP" ]; then
  echo "THREADS=$THREADS exceeds floor(176/$N)=$CAP (PROTOCOL 6). Refusing to launch."; exit 1
fi

mkdir -p logs "psi_cache_${ARM}"
for i in $(seq 0 $((N-1))); do
  env PSI_ARM="$ARM" PSI_SEEDS="$SEEDS" PSI_SHARD="$i" PSI_NSHARD="$N" \
      PSI_GEOM="${PSI_GEOM:-p000}" PSI_TRUNK="${PSI_TRUNK:-lstm}" \
      PSI_FEATURE_SET="${PSI_FEATURE_SET:-v7.3}" \
      PSI_EPOCH_SELECT="${PSI_EPOCH_SELECT:-0}" \
      PSI_LEAD="${PSI_LEAD:-0}" PSI_EXCLUDE="${PSI_EXCLUDE:-}" \
      PSI_SEED_BASE="${PSI_SEED_BASE:-}" \
      MOSAIC_PSI_CORE_BUDGET="$THREADS" MOSAIC_PSI_TF_INTRAOP="$THREADS" \
      MOSAIC_PSI_TF_INTEROP=2 OMP_NUM_THREADS="$THREADS" \
      OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
      nohup "$HOME/bin/r-mosaic-Rscript" run_arm.R \
      > "logs/${ARM}_shard${i}.log" 2>&1 &
  sleep 1
done
sleep 10
echo "ARM=$ARM shards=$N seeds=$SEEDS threads=$THREADS geom=${PSI_GEOM:-p000} trunk=${PSI_TRUNK:-lstm} feat=${PSI_FEATURE_SET:-v7.3} epoch_select=${PSI_EPOCH_SELECT:-0}"
echo "running processes: $(pgrep -fc 'run_arm.R')"
echo
echo "VERIFY BEFORE WALKING AWAY (wave-4 lesson): the inner-fold count printed in"
echo "  logs/${ARM}_shard0.log must match PROTOCOL section 6's table for this geometry."
grep -m1 "inner folds" "logs/${ARM}_shard0.log" 2>/dev/null || true
