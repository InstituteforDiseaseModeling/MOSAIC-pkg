#!/usr/bin/env bash
# dugong_power.sh -- start, stop and check the dugong Azure VM, through the admin server.
#
#   vm/dugong_power.sh status          is dugong up? (exit 0 up, 1 not reachable)
#   vm/dugong_power.sh up              start it and wait until SSH answers
#   vm/dugong_power.sh down [--force]  check it is idle, deallocate it, wait until SSH stops answering
#
# Starting and stopping run the admin server's own scripts from your home directory there:
# ./dugongup (az vm start) and ./dugongdown (az vm deallocate, which stops the compute billing).
# This file holds no hosts, users or credentials. It reaches both machines only through SSH
# aliases in your ~/.ssh/config, with key login only (BatchMode), so it never prompts: a missing
# key or alias fails at once. Settings (environment variables):
#   DUGONG_HOST          alias of dugong                                  (default: dugong)
#   DUGONG_ADMIN_HOST    alias of the admin server                        (default: dugong-admin)
#   DUGONG_UP_TIMEOUT    seconds to wait for SSH after the start          (default: 600)
#   DUGONG_DOWN_TIMEOUT  seconds to wait for SSH to stop answering after  (default: 180)
#
# up    does nothing if dugong already answers.
# down  refuses (exit 2) while an R process runs on dugong or anyone is logged in, and when dugong
#       cannot be reached to check; --force skips the check (it may kill someone's run).
# Only one up/down runs at a time on this machine (a lock in $TMPDIR).
#
# Exit codes: 0 done; 1 failed (or, for status, not reachable); 2 refused; 64 usage.
set -uo pipefail

DUGONG_HOST="${DUGONG_HOST:-dugong}"
DUGONG_ADMIN_HOST="${DUGONG_ADMIN_HOST:-dugong-admin}"
UP_TIMEOUT="${DUGONG_UP_TIMEOUT:-600}"
DOWN_TIMEOUT="${DUGONG_DOWN_TIMEOUT:-180}"
POLL=15
SSH_OPTS=(-o BatchMode=yes -o ConnectTimeout=10 -o ServerAliveInterval=30 -o ServerAliveCountMax=4)

say()  { printf '%s  %s\n' "$(date -u '+%H:%M:%S UTC')" "$*"; }
fail() { local code=${2:-1}; say "ERROR: $1" >&2; exit "$code"; }
usage() { sed -n '2,6p' "$0" | sed 's/^# \{0,1\}//' >&2; exit 64; }

# Aliases only: a value starting with "-" would be read by ssh as an option.
for v in "$DUGONG_HOST" "$DUGONG_ADMIN_HOST"; do
  [[ "$v" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] || fail "not an SSH alias: '$v'" 64
done
for v in "$UP_TIMEOUT" "$DOWN_TIMEOUT"; do
  [[ "$v" =~ ^[0-9]+$ ]] || fail "timeouts must be whole seconds, got '$v'" 64
done

reachable() { ssh "${SSH_OPTS[@]}" "$DUGONG_HOST" true >/dev/null 2>&1; }

# Run one of the admin server's scripts (fixed names, no arguments) and show its output, minus the
# admin server's post-quantum key-exchange notice.
admin_run() {
  local out rc
  out=$(ssh "${SSH_OPTS[@]}" "$DUGONG_ADMIN_HOST" "cd ~ && ./$1" 2>&1); rc=$?
  printf '%s\n' "$out" | grep -v -E 'post-quantum|store now, decrypt later|openssh.com/pq.html' | sed '/^$/d'
  return "$rc"
}

# R processes (by process name, so the check never matches itself) and logged-in users on dugong.
activity() {
  ssh "${SSH_OPTS[@]}" "$DUGONG_HOST" 'bash -s' <<'EOF'
ps -eo user=,pid=,etime=,comm= | awk '$4 == "R" || $4 == "Rscript" { print "R process: " $0 }'
who | awk '{ print "logged in: " $0 }'
EOF
}

lock() {
  LOCK="${TMPDIR:-/tmp}/dugong_power.lock"
  mkdir "$LOCK" 2>/dev/null || fail "another dugong_power.sh up/down is running (lock $LOCK; remove it if stale)"
  trap 'rmdir "$LOCK" 2>/dev/null' EXIT
}

wait_for() {   # wait_for up|down seconds
  local want=$1 limit=$2 t0=$SECONDS
  while (( SECONDS - t0 < limit )); do
    if [[ $want == up ]] && reachable; then return 0; fi
    if [[ $want == down ]] && ! reachable; then return 0; fi
    sleep "$POLL"
  done
  return 1
}

cmd_status() {
  if reachable; then
    say "dugong is UP"
    ssh "${SSH_OPTS[@]}" "$DUGONG_HOST" 'echo "  $(hostname): $(uptime)"'
    activity | sed 's/^/  /'
    return 0
  fi
  say "dugong is NOT REACHABLE: stopped (deallocated), still starting, or your network is not allowed to reach it"
  return 1
}

cmd_up() {
  lock
  if reachable; then say "dugong is already up; nothing to do"; cmd_status; return 0; fi
  say "starting dugong: ssh $DUGONG_ADMIN_HOST ./dugongup (about 2-3 minutes)"
  admin_run dugongup || fail "./dugongup failed (output above); dugong was not started"
  say "Azure reports dugong started; waiting up to ${UP_TIMEOUT}s for SSH"
  wait_for up "$UP_TIMEOUT" ||
    fail "dugong started but SSH did not answer within ${UP_TIMEOUT}s: still booting, its IP changed, or your network is not allowed. It is running (and billed): stop it with '$0 down --force' if you cannot reach it."
  cmd_status
}

cmd_down() {
  local force=$1 act
  lock
  if reachable; then
    act=$(activity) || fail "could not check dugong for running work"
    if [[ -n "$act" ]]; then
      printf '%s\n' "$act" | sed 's/^/  /'
      if [[ $force == yes ]]; then say "--force: stopping dugong despite the activity above"
      else fail "dugong is in use (above); not stopping it. Use --force only if that work may be killed." 2; fi
    else
      say "dugong is idle: no R processes, nobody logged in"
    fi
  elif [[ $force == yes ]]; then
    say "dugong is not reachable, so it cannot be checked; --force: deallocating anyway"
  else
    fail "dugong is not reachable, so it cannot be checked for running work (already stopped, or your network is not allowed). Use --force to deallocate it anyway." 2
  fi
  say "stopping dugong: ssh $DUGONG_ADMIN_HOST ./dugongdown (about 1-2 minutes)"
  admin_run dugongdown || fail "./dugongdown failed (output above); dugong may still be running"
  say "Azure reports dugong deallocated; checking SSH no longer answers"
  wait_for down "$DOWN_TIMEOUT" || fail "./dugongdown succeeded but dugong still answers on SSH after ${DOWN_TIMEOUT}s"
  say "dugong is DOWN"
}

case "${1:-}" in
  status) [[ $# -eq 1 ]] || usage; cmd_status ;;
  up)     [[ $# -eq 1 ]] || usage; cmd_up ;;
  down)   case "${2:-}" in
            "")      [[ $# -eq 1 ]] || usage; cmd_down no ;;
            --force) [[ $# -eq 2 ]] || usage; cmd_down yes ;;
            *)       usage ;;
          esac ;;
  *)      usage ;;
esac
