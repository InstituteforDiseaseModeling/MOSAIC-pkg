#!/usr/bin/env bash
# Run when P000/P000R have finished: score both, measure the FLOOR, then launch
# the country-variability arms N8 and N5.
#
# PROTOCOL order is enforced here so it cannot be skipped: no N-arm launches
# until the floor for class R at n_seeds=10 exists (section 3b), because every
# refit-arm delta is uninterpretable without it -- the single lesson of waves 0-20.
set -u
cd "$HOME/psi_evolve" || exit 1
[ -f STOP ] && { echo "STOP present -- halting (PROTOCOL 6)."; exit 1; }
if [ "$(pgrep -fc '[r]un_arm.R')" != "0" ]; then
  echo "arms still running -- refusing to score or launch."; exit 1
fi

echo "=================== 1. SCORE P000 and P000R ==================="
for A in P000 P000R; do
  n=$(ls psi_cache_$A/psi_*.csv 2>/dev/null | wc -l)
  echo "--- $A ($n/9 cutoffs cached) ---"
  [ "$n" -lt 6 ] && { echo "  too few cutoffs to score selection; investigate."; continue; }
  PSI_ARM=$A PSI_MODE=selection PSI_CACHE="$HOME/psi_evolve/psi_cache_$A" \
    "$HOME/bin/r-mosaic-Rscript" score_arm_driver.R 2>&1 | \
    grep -aE "^\[|BASELINE|A6|persistence|seasonal|per horizon|h1mo|MISSING" | head -14
done

echo
echo "=================== 2. THE FLOOR =============================="
"$HOME/bin/r-mosaic-Rscript" compare_arms.R \
  score_P000R_selection_seed_psi.rds score_P000_selection_seed_psi.rds --floor \
  2>&1 | tail -25

echo
echo "=================== 3. P000 vs P001 (incumbent) ==============="
"$HOME/bin/r-mosaic-Rscript" compare_arms.R \
  score_P000_selection_seed_psi.rds score_P001_selection_seed_psi.rds 2>&1 | \
  grep -aE "^(S\(|dS|A1|A2|A3|A6|==>)|exact|bootstrap|guard" | head -12

echo
echo "NEXT: read the floor above, then launch with it in hand:"
echo "  PSI_COUNTRY_BALANCE=1 ./launch_arm.sh N8 9 10 9"
echo "  PSI_FILM_INPUT=1      ./launch_arm.sh N5 9 10 9"
