---
name: data-object-rebuild-traps
description: Traps when rebuilding priors_default/config_default and their dependents from a git worktree on the shared laptop - stale default-library MOSAIC (builders, tests and PSOCK workers), a shared private rlib reinstalled under another agent, MODEL_INPUT/DOCS_* pointing at other trees, the quiet-start placeholder, same-day byte identity, false diffs, two priors passes on a window move, and the ASCII-only roxygen the PDF manual needs
metadata:
  type: reference
---

Learned on the v17.1 priors rebuild (2026-10-01, worktree v0101-trust).

- **Default library MOSAIC is stale.** The laptop's user library held MOSAIC 0.91.14 while the branch was
  0.101.0. `data-raw/make_priors_default.R` starts with `library(MOSAIC)` and reads
  `MOSAIC::config_default` and every estimator from the INSTALLED package, so without a private library the
  build silently runs nine-month-old code. Install with `R CMD INSTALL --library=<rlib> <worktree>`, which
  works from a pure-R source dir without writing into it, and run builders with `R_LIBS=<rlib>`. Assert
  `find.package("MOSAIC")` inside the run.
- **get_paths() points two entries outside the tree.** `MODEL_INPUT`/`MODEL_OUTPUT` resolve to the canonical
  checkout; the builders re-point only those two from `getwd()`. `DOCS_FIGURES` still goes to MOSAIC-docs:
  `est_kappa_prior()` and the three `est_zeta_*_prior()` called by the priors builder write PNGs there. They
  also rewrite their `model/input/*_kappa|zeta_*.csv`, but byte-identically. To redirect without editing
  the builder, source a temp copy with two `PATHS$DOCS_*` lines injected after its
  `PATHS$MODEL_OUTPUT <- ...` line. The wrapper is in claude/v0101_rebuild/phaseA/build_priors.R.
- **`{quiet_start_seeded}` placeholder.** The builder `sub()`s only its FIRST occurrence in the
  description. When you add a new changelog head, put the placeholder in the new head and freeze the
  previous entry's text to its literal list. Otherwise the old entry renders the new build's list, or the
  new head shows a raw `{...}`.
- **Byte identity needs the same day.** `metadata$date = Sys.Date()`, and the changelog-head date must
  match it, otherwise the builder warns. Do both builds before midnight. On a load-30 laptop one priors
  build takes about 6 min and `est_seasonal_dynamics()` about 0.4 min.
- **Surveillance fixes move priors you didn't touch.** The E/I window priors and the quiet-start set read
  the daily combined file, which includes AI rows (not filtered). `epidemic_threshold` uses every non-AI
  outbreak week. The v0.101.0 gap-only reconciliation removed AI Fourier rows from the IC window, which
  made SSD/TZA/UGA/ZAF/ZWE quiet starts. ZAF's curated window gave it 26 outbreak weeks, which pushed it off
  the Zheng fallback and cut its threshold x0.009. Diff every parameter and attribute each change from the
  two data versions; `attrib_priors.R` in the same folder does that.

From the config v6.1 / Phase B pass (same day):
- **`devtools::test()` needs `R_LIBS=<rlib>` too.** load_all serves the parent process, but PSOCK workers
  started by tests or by `run_MOSAIC()` call `library(MOSAIC)` and would load the stale default-library
  build. Reinstall the private library from HEAD before any suite, smoke or integration run.
- **Dependent objects have false diffs.** `make_estimated_parameters_inventory.R` stamps
  `attr(, "creation_date") <- Sys.Date()`, so the .rda changes daily with identical content. Compare
  with that attribute dropped and don't commit a date-only change. The toy builders `print()` a figure,
  which leaves `Rplots.pdf` in the package root under Rscript, so run them with `pdf(NULL)` open.
- **Expected check noise.** `testthat::test_file()` skips `skip_on_cran()` tests unless `NOT_CRAN=true`.
  The config JSON round trip loses dimnames on the weight and tier matrices, so `all.equal()` reports
  attribute mismatches while the values are equal. The engine's `coupling` channel is a Pearson matrix
  with NaN and negative values by contract, so exclude it from finite/non-negative smoke checks.
- **A config with `reported_tier` breaks tier-free baselines.** Since v6.1 the shipped config carries
  tiers, so a test that used `config_default` as its no-tier baseline must drop the field explicitly.

From the 2018-window dry run (v0.103.0, 2026-10-03):
- **A window move needs TWO priors passes.** `est_initial_V1_V2()` divides doses by the INSTALLED
  `config$N_j_initial` (E/I and R read UN WPP population at t0). Pass 1 runs against the old config, so
  2018 V1/V2 were divided by 2023 population: pass 2 (new config installed) moved prop_V1 x1.11-1.19 in
  13 locations, prop_V2 in NGA/ZMB and the residual prop_S; every other leaf was byte-identical, and N
  does not depend on priors, so pass 2 is the fixed point. Recipe: priors, install, config, install,
  priors again, install, config again; byte-compare pass 2 against pass 3.
- **Both builders now guard the build order.** The priors builder stops when the env var is unset and
  the installed `config_default$date_start` differs from the config builder's default literal.
- **Dry-run pattern:** `git archive <commit>` into `claude/`, give the builders `"-dry"` version
  strings (only informational, so run_MOSAIC and tests accept them, and a rubric PROVENANCE check
  would reject them), build against the private library, run `devtools::test()` from the snapshot.
  The worktree stays clean, and nothing dry can be committed by accident.
- **The plan's test inventory missed five tests.** Synthetic psi caches predicted from a hard-coded
  2023-01-01 (run_rolling_cv rejects a cache that starts after the config), a `date_start` override that
  left `date_stop` alone, and a "MOZ peak on day 86" that silently skipped. Grep for literal dates is
  not enough: run the suite on a dry build and diff the skip list too.

From the v0.103.0 red-team round (2026-10-03):
- **A private library stays private only if one task uses it.** Another agent loaded MOSAIC from
  `claude/v0103_rebuild/rlib` while I reinstalled into it; during `R CMD INSTALL` the package directory
  is briefly absent, so its process fell through to the default library's 0.91.14. Give each task its
  own library (`rlib_<task>`) and assert `find.package()` in every script that loads from it.
- **The PDF manual needs ASCII roxygen maths.** Greek letters, U+2248/2265/2260 and subscript digits
  break `R CMD Rd2pdf` (`--no-manual` checks hide it). Use `\eqn{\beta}{beta}` forms or words.
  Under `Roxygen: list(markdown = TRUE)` a bare `[\code{x}, T]` becomes a broken `\link{}`: write
  `\[...\]`. Verify with `R CMD Rd2pdf --no-preview --batch` on a copy of `DESCRIPTION` + `man/`, and
  grep the log with `grep -a` (it holds binary bytes, so plain grep prints nothing).

Related: [[production-suite-protocol]], [[run-id-suffix-and-unkeyed-cache]].
