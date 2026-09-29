#!/usr/bin/env bash
# =============================================================================
# psi_ab/launch.sh -- run the psi A/B screens on dugong.
#
# GRID: whatever run.R's SPEC defines (derived via PSI_AB_PHASE=grid below):
#       6 arms x 4 units x 3 cutoffs = 72 cells; completed cells are skipped.
#
# No prefit phase: the psi caches were built by build_caches.R and are frozen.
# The queue is drawn from the FLAT 36-cell pool rather than per-arm lanes, so a
# slow COD cell never idles cores a fast MOZ cell could use.
#
# Resume-safe: run.R skips any cell that already has predictions.parquet, so
# the timing-probe cell is skipped automatically and a crashed run can simply
# be relaunched.
#
# Survives SSH disconnect:
#   scp claude/psi_ab/{run.R,launch.sh} dugong:~/
#   ssh dugong 'cd ~ && nohup bash ~/launch.sh </dev/null >psi_ab.log 2>&1 & echo $!'
#
# Scoring runs on the LAPTOP (score.R), not here.
# =============================================================================
set -u
cd ~
SCRIPT="$HOME/run.R"
WRAP="$HOME/bin/r-mosaic-Rscript"        # libexpat LD_PRELOAD wrapper (dugong)
LOGDIR="$HOME/psi_ab_logs"; mkdir -p "$LOGDIR"

export MOSAIC_ROOT="${MOSAIC_ROOT:-$HOME/MOSAIC}"
export PSI_AB_DRYRUN=0
CELL_CORES="${CELL_CORES:-43}"           # 4 x 43 = 172 of 176
MAXJOBS="${MAXJOBS:-4}"

[ -x "$WRAP" ]   || { echo "FATAL: wrapper $WRAP not found"; exit 1; }
[ -f "$SCRIPT" ] || { echo "FATAL: run script $SCRIPT not found"; exit 1; }
for a in prod nd nd_rep p000 p000r n9; do
     [ -f "$HOME/psi_ab/cache_$a/psi_manifest.json" ] || {
          echo "FATAL: psi cache missing for arm $a (run build_caches.R first)"; exit 1; }
done
[ -f "$HOME/psi_ab/arm_specs.rds" ] || { echo "FATAL: arm_specs.rds missing"; exit 1; }

# Validate the whole grid with no compute before burning a single cell.
PSI_AB_DRYRUN=1 "$WRAP" "$SCRIPT" >"$LOGDIR/preflight.log" 2>&1 \
     || { echo "FATAL: pre-flight dry run failed; see $LOGDIR/preflight.log"; exit 1; }
echo "[$(date '+%F %T')] pre-flight OK"

# The grid is DERIVED from run.R's SPEC, never duplicated here. An earlier
# version hardcoded the cutoff list, drifted from SPEC, and launched cells at
# an excluded cutoff while skipping a required one -- caught only by reading
# the launch log. run.R PHASE=grid is now the single source of truth.
mapfile -t CELLS < <(PSI_AB_DRYRUN=1 PSI_AB_PHASE=grid "$WRAP" "$SCRIPT" 2>/dev/null \
                       | grep -E '^[a-z0-9_]+:[A-Z+]+:[0-9]{4}-[0-9]{2}-[0-9]{2}$')
# Arm names may contain digits (p000, n9) -- an earlier [a-z_]+ pattern would
# have silently dropped every such cell and reported the grid complete.
[ "${#CELLS[@]}" -gt 0 ] || { echo "FATAL: could not derive the cell grid from $SCRIPT"; exit 1; }
echo "[$(date '+%F %T')] START ${#CELLS[@]} cells, ${MAXJOBS}-wide, ${CELL_CORES} cores/cell"
printf '  cell: %s\n' "${CELLS[@]}"

for cell in "${CELLS[@]}"; do
     A="${cell%%:*}"; rest="${cell#*:}"; U="${rest%%:*}"; T="${rest##*:}"
     while [ "$(jobs -rp | wc -l)" -ge "$MAXJOBS" ]; do wait -n; done
     echo "[$(date '+%F %T')] launch $A / $U @ $T"
     PSI_AB_PHASE=calibrate PSI_AB_ARM="$A" PSI_AB_UNIT="$U" PSI_AB_CUTOFF="$T" \
       PSI_AB_CORES="$CELL_CORES" \
       "$WRAP" "$SCRIPT" >"$LOGDIR/cal_${A}_${U}_${T}.log" 2>&1 &
done
wait
echo "[$(date '+%F %T')] all cells done."

DONE=$(ls "$HOME"/MOSAIC/MOSAIC-pkg/claude/psi_ab/out/per_arm/*/per_unit/*/cutoff_*/predictions.parquet 2>/dev/null | wc -l)
echo "[$(date '+%F %T')] predictions.parquet present: $DONE / ${#CELLS[@]}"
[ "$DONE" -eq "${#CELLS[@]}" ] || echo "WARNING: incomplete -- inspect $LOGDIR/cal_*.log"
echo "NEXT: pull out/ to the laptop and run claude/psi_ab/score.R there."
