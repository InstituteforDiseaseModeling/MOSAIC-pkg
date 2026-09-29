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
#   PSI_FILM_INPUT=1                   N5: condition the trunk's INPUTS
#   PSI_GAMMA_SCALE=2                  N6: let country modulation flip sign
#   PSI_COUNTRY_BALANCE=1              N8: per-country loss balancing
#   PSI_COUNTRY_STATIC=frozen          D9b: country embedding from covariates
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
#   PSI_COUNTRY_BALANCE=1 ./launch_arm.sh N8 9 10 18          # cheapest country arm
#   PSI_FILM_INPUT=1 ./launch_arm.sh N5 9 10 18               # input-FiLM
#   PSI_GAMMA_SCALE=2 ./launch_arm.sh N6 9 10 18              # sign-permissive
set -u
ARM=${1:-P000}; N=${2:-9}; SEEDS=${3:-10}; THREADS=${4:-9}
# SHARE THE BOX. dugong is also used for MOSAIC calibrations by other work, so
# psi arms run at low priority and never claim the whole machine: a nice-0
# calibration preempts a nice-15 arm, and the arm soaks up whatever is idle.
# Measured 2026-09-18: 18 arm processes at THREADS=9 drew 94 of 176 cores and
# 93 GB of 1511 GB, so memory is never the constraint -- cores are.
# The user has mandated a standing 40-core reservation for another agent's
# MOSAIC calibrations; PSI_RESERVE_CORES enforces it at the launch boundary.
NICE=${PSI_NICE:-15}
# PRIVATE R LIBRARY. dugong's ~/R/library is shared, and on 2026-09-18 another
# agent's install downgraded MOSAIC there from 0.91.12 to 0.90.5 UNDER A LIVE
# WAVE -- killing P000H at launch with "unused arguments (step_days, test_days,
# min_train_years)" because the older .psi_make_rw_cv_steps has no day-based
# knobs. Arms already running were unharmed (they had loaded the newer package
# at startup, and their manifests record mosaic_version 0.91.12), but every new
# process was broken. Prepending a private lib makes psi arms immune to whatever
# else is installed in the shared one; dependencies still resolve behind it.
PSI_LIB=${PSI_LIB:-$HOME/Rlib_psi}
cd "$HOME/psi_evolve" || exit 1

# PROTOCOL section 6 kill switch: agents check for STOP before any launch.
if [ -f STOP ]; then echo "STOP file present -- refusing to launch (PROTOCOL 6)."; exit 1; fi

# Concurrency cap: leave at least a quarter of the box for other users, so the
# budget is floor(132 / n_processes) rather than floor(176 / n_processes).
RESERVE=${PSI_RESERVE_CORES:-40}   # user-mandated reservation for another agent
CAP=$(( (176 - RESERVE) / N ))
if [ "$THREADS" -gt "$CAP" ]; then
  echo "THREADS=$THREADS exceeds floor((176-$RESERVE)/$N)=$CAP -- that would leave under"
  echo "$RESERVE cores for other work (PROTOCOL 6). Refusing to launch."; exit 1
fi
# Refuse to pile on if the box is already busy with someone else's work.
OTHER=$(ps -eo pcpu,args --sort=-pcpu | grep -v "[r]un_arm.R" | awk 'NR>1 && $1>50 {c+=$1} END {printf "%.0f", c/100}')
if [ "${OTHER:-0}" -gt 60 ]; then
  echo "another workload is using ~${OTHER} cores; refusing to launch on top of it."; exit 1
fi

mkdir -p logs "psi_cache_${ARM}"
for i in $(seq 0 $((N-1))); do
  env PSI_ARM="$ARM" PSI_SEEDS="$SEEDS" PSI_SHARD="$i" PSI_NSHARD="$N" \
      PSI_GEOM="${PSI_GEOM:-p000}" PSI_TRUNK="${PSI_TRUNK:-lstm}" \
      PSI_FILM_INPUT="${PSI_FILM_INPUT:-0}" \
      PSI_GAMMA_SCALE="${PSI_GAMMA_SCALE:-}" \
      PSI_COUNTRY_BALANCE="${PSI_COUNTRY_BALANCE:-0}" \
      PSI_COUNTRY_STATIC="${PSI_COUNTRY_STATIC:-}" \
      PSI_FEATURE_SET="${PSI_FEATURE_SET:-v7.3}" \
      PSI_EPOCH_SELECT="${PSI_EPOCH_SELECT:-0}" \
      PSI_LEAD="${PSI_LEAD:-0}" PSI_EXCLUDE="${PSI_EXCLUDE:-}" \
      PSI_SEED_BASE="${PSI_SEED_BASE:-}" \
      PSI_SMOKE="${PSI_SMOKE:-0}" \
      PSI_DLIN_KERNEL="${PSI_DLIN_KERNEL:-}" PSI_DLIN_PAD="${PSI_DLIN_PAD:-}" \
      PSI_DLIN_L2="${PSI_DLIN_L2:-}" PSI_DLIN_INDIVIDUAL="${PSI_DLIN_INDIVIDUAL:-0}" \
      MOSAIC_PSI_CORE_BUDGET="$THREADS" MOSAIC_PSI_TF_INTRAOP="$THREADS" \
      MOSAIC_PSI_TF_INTEROP=2 OMP_NUM_THREADS="$THREADS" \
      OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
      R_LIBS="$PSI_LIB:$HOME/R/library" \
      nohup nice -n "$NICE" "$HOME/bin/r-mosaic-Rscript" run_arm.R \
      > "logs/${ARM}_shard${i}.log" 2>&1 &
  sleep 1
done
sleep 10
echo "ARM=$ARM shards=$N seeds=$SEEDS threads=$THREADS geom=${PSI_GEOM:-p000} trunk=${PSI_TRUNK:-lstm} feat=${PSI_FEATURE_SET:-v7.3} epoch_select=${PSI_EPOCH_SELECT:-0}"
echo "running processes: $(pgrep -fc 'run_arm.R')  (nice $NICE, reserving >= $RESERVE cores)"
echo
echo "VERIFY BEFORE WALKING AWAY (wave-4 lesson): the inner-fold count printed in"
echo "  logs/${ARM}_shard0.log must match PROTOCOL section 6's table for this geometry."
grep -m1 "inner folds" "logs/${ARM}_shard0.log" 2>/dev/null || true
