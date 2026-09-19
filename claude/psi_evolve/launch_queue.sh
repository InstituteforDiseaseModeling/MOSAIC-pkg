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

for SPEC in "F6:PSI_GEOM=F6" "F5:PSI_GEOM=F5" "N9:PSI_COUNTRY_STATIC=frozen PSI_COUNTRY_BALANCE=1"; do
  ARM="${SPEC%%:*}"; ENVS="${SPEC#*:}"
  [ -f STOP ] && { echo "STOP present; queue halted"; exit 1; }
  # already complete? skip
  n=$(ls "psi_cache_${ARM}"/psi_*.csv 2>/dev/null | wc -l)
  if [ "$n" -ge 9 ]; then echo "$(date -Is) $ARM already has $n/9 -- skipping"; continue; fi
  echo "$(date -Is) waiting for capacity to launch $ARM (need other-workload < ${THRESH} cores)"
  while [ "$(free_cores)" -gt "$THRESH" ]; do sleep 300; done
  echo "$(date -Is) capacity available -- launching $ARM with: $ENVS"
  env $ENVS PSI_EPOCH_SELECT=2 ./launch_arm.sh "$ARM" 9 10 12 || {
      echo "$(date -Is) launch of $ARM refused; will retry next cycle"; sleep 600; continue; }
  # wait for this arm to finish before starting the next
  sleep 120
  while [ "$(pgrep -fc '[r]un_arm.R')" -gt 0 ]; do sleep 300; done
  echo "$(date -Is) $ARM finished: $(ls psi_cache_${ARM}/psi_*.csv 2>/dev/null | wc -l)/9 cutoffs"
done
echo "$(date -Is) queue complete"
