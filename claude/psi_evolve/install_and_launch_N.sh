#!/usr/bin/env bash
# Install the committed MOSAIC (with N5/N6/N8) on dugong, then launch N8 + N5.
# NOT run while arms are live: overwriting a package that running R processes
# have lazy-loaded can corrupt their load database.
set -eu
SHA="${1:-feature/psi-12wk-evolve}"
if [ "$(pgrep -fc '[r]un_arm.R')" != "0" ]; then
  echo "arms still running -- refusing to reinstall MOSAIC underneath them."; exit 1
fi
cd "$HOME" && rm -rf mosaic_build && mkdir -p mosaic_build && cd mosaic_build
git clone -q --depth 1 --branch feature/psi-12wk-evolve \
  https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg.git
cd MOSAIC-pkg && git rev-parse --short HEAD && grep '^Version:' DESCRIPTION
# NOTE (2026-09-23): this installed into the SHARED ~/R/library, which is NOT
# the library launch_arm.sh loads from ($HOME/Rlib_psi is prepended). So this
# script's "installed:" line could report a version no arm was actually using,
# and it wrote to a library another agent shares. Fixed to the private lib;
# use install_mosaic.sh for a plain install+verify.
cd .. && R_LIBS="$HOME/Rlib_psi:$HOME/R/library" \
  R CMD INSTALL --no-multiarch --no-docs -l "$HOME/Rlib_psi" MOSAIC-pkg 2>&1 | tail -3
"$HOME/bin/r-mosaic-Rscript" -e '
cat("installed:", as.character(packageVersion("MOSAIC")), "\n")
ac <- MOSAIC:::.psi_load_arch_control(list(film_input=TRUE, country_balance=TRUE))
cat("film_input knob:", isTRUE(ac$film_input),
    " country_balance knob:", isTRUE(ac$country_balance), "\n")'
cd "$HOME/psi_evolve"
echo "--- launching N8 (country_balance) ---"
PSI_COUNTRY_BALANCE=1 ./launch_arm.sh N8 9 10 9 2>&1 | tail -3
echo "--- launching N5 (input-FiLM) ---"
PSI_FILM_INPUT=1 ./launch_arm.sh N5 9 10 9 2>&1 | tail -3
echo "total procs: $(pgrep -fc '[r]un_arm.R')"
