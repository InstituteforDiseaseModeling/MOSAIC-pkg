#!/usr/bin/env bash
# Launch one arm across N sharded processes on dugong.
# usage: launch_arm.sh ARM NSHARD NSEEDS [THREADS]
set -u
ARM=${1:-A000}; N=${2:-9}; SEEDS=${3:-3}; THREADS=${4:-16}
cd "$HOME/psi_evolve" || exit 1
mkdir -p logs "psi_cache_${ARM}"
for i in $(seq 0 $((N-1))); do
  env PSI_ARM="$ARM" PSI_SEEDS="$SEEDS" PSI_SHARD="$i" PSI_NSHARD="$N" \
      MOSAIC_PSI_CORE_BUDGET="$THREADS" MOSAIC_PSI_TF_INTRAOP="$THREADS" \
      MOSAIC_PSI_TF_INTEROP=2 OMP_NUM_THREADS="$THREADS" \
      OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1 NUMEXPR_NUM_THREADS=1 \
      nohup "$HOME/bin/r-mosaic-Rscript" run_arm.R \
      > "logs/${ARM}_shard${i}.log" 2>&1 &
  sleep 1
done
sleep 10
echo "ARM=$ARM shards=$N seeds=$SEEDS threads=$THREADS"
echo "running processes: $(pgrep -fc 'run_arm.R')"
