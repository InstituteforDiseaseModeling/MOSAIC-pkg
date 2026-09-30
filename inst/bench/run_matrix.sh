#!/bin/sh
# =============================================================================
# run_matrix.sh -- interleaved multi-arm benchmark driver.
#
# Runs every arm inside every block, rotating arm order between blocks, so that
# thermal drift and background load fall on all arms equally. Blocking by arm
# instead (all of arm A, then all of arm B) confounds the comparison with
# machine drift: measured drift on an identical binary reached 7.8% over 20
# minutes, which is larger than most effects worth tracking.
#
#   sh inst/bench/run_matrix.sh <out.csv> <n_blocks>
#
# Arms are declared as "name:libpath:srcdir".
# =============================================================================
set -e
OUT=${1:-/tmp/claude-501/bench_matrix.csv}
NB=${2:-4}
HERE=$(cd "$(dirname "$0")" && pwd)
RUN="$HERE/run_bench.R"

ARMS="main:/tmp/claude-501/libmain:/tmp/claude-501/mainbl \
pr122:/tmp/claude-501/pr122/MOSAIC.Rcheck:/tmp/claude-501/pr122 \
pr123:/tmp/claude-501/lib123:/tmp/claude-501/pr123"

: > "$OUT"
b=1
while [ "$b" -le "$NB" ]; do
  # rotate arm order each block
  i=0
  for spec in $ARMS; do
    i=$((i+1))
    if [ $(( (i + b) % 3 )) -eq 0 ]; then FIRST="$spec"; fi
  done
  ORDER=""
  for spec in $ARMS; do [ "$spec" = "$FIRST" ] && ORDER="$spec $ORDER" || ORDER="$ORDER $spec"; done
  for spec in $ORDER; do
    arm=$(echo "$spec" | cut -d: -f1)
    lib=$(echo "$spec" | cut -d: -f2)
    src=$(echo "$spec" | cut -d: -f3)
    echo "--- block $b / arm $arm ---"
    Rscript "$RUN" --lib="$lib" --arm="$arm" --src="$src" --block="$b" --out="$OUT" \
      2>&1 | grep -E "min=|SKIP|FAIL|rows" || true
  done
  b=$((b+1))
done
echo "matrix complete -> $OUT"
