#!/usr/bin/env bash
# Wait for the box to free up, then launch the queued arms one at a time.
#
# The other agent's MOSAIC calibration scaled to ~150 of 176 cores. launch_arm.sh
# refuses to start on top of a workload >60 cores, which is correct -- so rather
# than override the guard, this waits for capacity and then launches.
#
# usage: nohup ./launch_queue.sh > logs/queue.log 2>&1 &
set -u
cd "$HOME/psi_evolve" || exit 1
THRESH=${PSI_QUEUE_THRESH:-60}

free_cores () {
  ps -eo pcpu,args --sort=-pcpu | grep -v "[r]un_arm.R" \
    | awk 'NR>1 && $1>50 {c+=$1} END {printf "%.0f", c/100}'
}

# Tier 1 first (cheapest and decisive), then Tier 2. T2 is the lead-12 arm --
# the one change phase 1 never tested cleanly -- and N9 is the first stacked arm
# (D9b level + N8 shape). F5/F6 are the inner-CV geometry arms.
for SPEC in "T2:PSI_LEAD=12" \
            "N9:PSI_COUNTRY_STATIC=frozen PSI_COUNTRY_BALANCE=1" \
            "ND:PSI_TRUNK=dlinear" \
            "NT:PSI_TRUNK=tcn" \
            "F5:PSI_GEOM=F5" \
            "F6:PSI_GEOM=F6"; do
  ARM="${SPEC%%:*}"; ENVS="${SPEC#*:}"
  [ -f STOP ] && { echo "STOP present; queue halted"; exit 1; }
  # already complete? skip
  n=$(ls "psi_cache_${ARM}"/psi_*.csv 2>/dev/null | wc -l)
  if [ "$n" -ge 10 ]; then echo "$(date -Is) $ARM already has $n/10 -- skipping"; continue; fi
  echo "$(date -Is) waiting for capacity to launch $ARM (need other-workload < ${THRESH} cores)"
  while [ "$(free_cores)" -gt "$THRESH" ]; do sleep 300; done
  echo "$(date -Is) capacity available -- launching $ARM with: $ENVS"
  env $ENVS PSI_EPOCH_SELECT=2 ./launch_arm.sh "$ARM" 10 10 12 || {
      echo "$(date -Is) launch of $ARM REFUSED -- skipping it (there is no outer retry loop; re-run this script to pick it up)"; continue; }
  # wait for this arm to finish before starting the next
  sleep 120
  while [ "$(pgrep -fc '[r]un_arm.R')" -gt 0 ]; do sleep 300; done
  echo "$(date -Is) $ARM finished: $(ls psi_cache_${ARM}/psi_*.csv 2>/dev/null | wc -l)/10 cutoffs"
done
echo "$(date -Is) queue complete"
