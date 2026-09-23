#!/usr/bin/env bash
# Install the committed MOSAIC into the PRIVATE psi library on dugong, then
# verify the two things the priority-set wave depends on.
#
# usage: ./install_mosaic.sh [branch-or-sha]
#
# WHY A PRIVATE LIBRARY. dugong's ~/R/library is shared, and on 2026-09-18
# another agent's install downgraded MOSAIC there from 0.91.12 to 0.90.5 UNDER
# A LIVE WAVE, breaking every newly-spawned arm. launch_arm.sh prepends
# $HOME/Rlib_psi for exactly this reason, so that is where we install. NEVER
# write to ~/R/library from here -- install_and_launch_N.sh used to, which is
# why its MOSAIC and the one the arms actually load had drifted apart.
set -eu
REF="${1:-feature/psi-12wk-evolve}"
PSI_LIB="$HOME/Rlib_psi"

if [ "$(pgrep -fc '[r]un_arm.R')" != "0" ]; then
  echo "arms still running -- refusing to reinstall MOSAIC underneath them."
  echo "(overwriting a package that running R processes have lazy-loaded can"
  echo " corrupt their load database)"; exit 1
fi

mkdir -p "$PSI_LIB"
cd "$HOME" && rm -rf mosaic_build && mkdir -p mosaic_build && cd mosaic_build
git clone -q --depth 1 --branch "$REF" \
  https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg.git
cd MOSAIC-pkg
echo "--- installing $(git rev-parse --short HEAD) $(grep '^Version:' DESCRIPTION) ---"
cd .. && R_LIBS="$PSI_LIB:$HOME/R/library" \
  R CMD INSTALL --no-multiarch --no-docs -l "$PSI_LIB" MOSAIC-pkg 2>&1 | tail -3

# VERIFY, do not assume. Each check is a thing a queued arm would otherwise die
# on (or, worse, silently run without).
R_LIBS="$PSI_LIB:$HOME/R/library" "$HOME/bin/r-mosaic-Rscript" -e '
lp <- find.package("MOSAIC")
cat("library      :", lp, "\n")
cat("version      :", as.character(utils::packageVersion("MOSAIC")), "\n")
fit <- paste(deparse(MOSAIC:::.psi_fit_predict_lstm), collapse = " ")
cat("trunk dlinear:", grepl("dlinear", fit), " (ND dies without it)\n")
cat("epoch fix    :", is.function(MOSAIC:::.psi_epoch_from_history), "\n")
h <- list(metrics = list(loss = rep(0, 25),
                         val_loss = c(seq(1, .5, length.out = 15), seq(.51, .6, length.out = 10))))
cat("  argmin(val_loss) =", MOSAIC:::.psi_epoch_from_history(h, TRUE),
    "(must be 15, not 25)\n")
ac <- MOSAIC:::.psi_load_arch_control(NULL)
cat("D9b default  :", ac$country_static, "\n")
cat("N8 default   :", isTRUE(ac$country_balance), "\n")
cat("anchor hook  :", "target_anchor_stop" %in% names(formals(MOSAIC::compile_suitability_data)), "\n")
stopifnot(grepl("dlinear", fit),
          MOSAIC:::.psi_epoch_from_history(h, TRUE) == 15L,
          identical(ac$country_static, "auto"),
          isTRUE(ac$country_balance))
cat("\nINSTALL VERIFIED\n")'
