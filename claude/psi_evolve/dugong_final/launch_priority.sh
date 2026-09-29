#!/usr/bin/env bash
# PRIORITY SET (2026-09-23). Supersedes launch_queue.sh for this wave.
#
# usage: nohup ./launch_priority.sh > logs/priority.log 2>&1 &
#
# WHAT CHANGED FROM launch_queue.sh, AND WHY
#
#   DROPPED  F5, F6 (8.5 h, 42% of the old queue). The inner-CV geometry axis is
#            CLOSED: four geometries spanning 89->912 folds landed within 2.2%
#            with no monotone relation. F5 completes a 2x2 whose cell-to-cell
#            differences (+1.8%, -2.1%) sit barely above the 0.5% "unresolved"
#            threshold, and they are differences in MAE -- the metric wave 34
#            showed a CONSTANT reproduces. Neither can move dir_acc, which
#            PLAN_PHASE2.md declares the primary axis. Deferred, not cancelled:
#            F6 is the better geometry and becomes worth running the moment
#            anything moves dir_acc off 0.5.
#
#   DROPPED  the block-10 backfill (6 h). The epoch fix (.psi_epoch_from_history)
#            changed round(median(best_epoch)) and therefore every psi, so the
#            15 caches already on disk are no longer valid comparators. Adding a
#            10th cutoff to them buys a comparison we cannot make. Arms below
#            launch with NSHARD=10 and so cover block 10 natively.
#
#   ADDED    P000E -- the re-baseline the epoch fix forces. NOT optional and NOT
#            written into psi_cache_P000: the old baseline is kept intact for
#            provenance, this is a new cache under the corrected code.
#
#   DEFERRED NT (tcn) to wave C, conditional on ND. If DLinear ties the LSTM the
#            question TCN answers is already answered.
#
# ORDER. ND first: measured at 4.7 min/cutoff in smoke (~20 min for the arm), so
# it validates the whole pipeline under the fresh build before anything expensive
# starts, and it is decisive on its own -- a linear trunk matching the 3-stack
# LSTM would say the architecture programme, phase 3 included, is misdirected.
# Then the baseline, then the two arms that need it.
set -u
cd "$HOME/psi_evolve" || exit 1
THRESH=${PSI_QUEUE_THRESH:-60}

free_cores () {
  ps -eo pcpu,args --sort=-pcpu | grep -v "[r]un_arm.R" \
    | awk 'NR>1 && $1>50 {c+=$1} END {printf "%.0f", c/100}'
}

# WAVE A is decisive; WAVE B validates a change already shipped to production
# (D9b + N8 went in as defaults on 2026-09-22 and have never been tested as a
# stack -- N9 is that test, and can only confirm or retract, not redirect).
for SPEC in "ND:PSI_TRUNK=dlinear" \
            "P000E:PSI_GEOM=p000" \
            "T2:PSI_LEAD=12" \
            "N9:PSI_COUNTRY_STATIC=frozen PSI_COUNTRY_BALANCE=1"; do
  ARM="${SPEC%%:*}"; ENVS="${SPEC#*:}"
  [ -f STOP ] && { echo "$(date -Is) STOP present; queue halted"; exit 1; }
  n=$(ls "psi_cache_${ARM}"/psi_*.csv 2>/dev/null | wc -l)
  if [ "$n" -ge 10 ]; then echo "$(date -Is) $ARM already has $n/10 -- skipping"; continue; fi
  echo "$(date -Is) waiting for capacity to launch $ARM (need other-workload < ${THRESH} cores)"
  while [ "$(free_cores)" -gt "$THRESH" ]; do sleep 300; done
  echo "$(date -Is) launching $ARM with: $ENVS"
  # 10 shards x 12 threads = 120 of 176 cores, so 56 stay free and the standing
  # 40-core reservation holds with margin.
  env $ENVS PSI_EPOCH_SELECT=2 ./launch_arm.sh "$ARM" 10 10 12 || {
      echo "$(date -Is) launch of $ARM REFUSED -- skipping (re-run this script to pick it up)"
      continue; }
  sleep 120
  while [ "$(pgrep -fc '[r]un_arm.R')" -gt 0 ]; do sleep 300; done
  echo "$(date -Is) $ARM finished: $(ls psi_cache_${ARM}/psi_*.csv 2>/dev/null | wc -l)/10 cutoffs"
done
echo "$(date -Is) priority set complete"
