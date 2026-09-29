#!/usr/bin/env bash
# Poll dugong with SHORT-LIVED ssh connections so a transient drop cannot end
# the watch (a single long-lived ssh returning 255 killed the first watcher).
# Exits when all cells are present OR the launcher is gone; always reports the
# error-log count so a crash is not mistaken for completion.
TARGET="${1:-72}"
fails=0
while true; do
  out=$(ssh -o ConnectTimeout=20 -o ServerAliveInterval=15 -o BatchMode=yes dugong \
        "echo \"\$(find ~/MOSAIC/MOSAIC-pkg/claude/psi_ab/out -name predictions.parquet 2>/dev/null | wc -l)|\$(ps -eo args | grep -c '^bash /home/jgiles/launch.sh')|\$(grep -ilE 'FAILED|^Error' ~/psi_ab_logs/cal_*.log 2>/dev/null | wc -l)\"" 2>/dev/null)
  if [ -z "$out" ]; then
    fails=$((fails+1))
    [ "$fails" -ge 12 ] && { echo "UNREACHABLE: 12 consecutive ssh failures"; exit 1; }
    sleep 60; continue
  fi
  fails=0
  done_n="${out%%|*}"; rest="${out#*|}"; launcher="${rest%%|*}"; errs="${rest##*|}"
  if [ "$done_n" -ge "$TARGET" ] || [ "$launcher" -eq 0 ]; then
    echo "GRID ENDED: complete=$done_n/$TARGET launcher=$launcher error_logs=$errs"
    exit 0
  fi
  sleep 180
done
