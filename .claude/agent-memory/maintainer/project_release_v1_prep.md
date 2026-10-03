---
name: release-v1-prep
description: MOSAIC v1.0 release branch (release/v1.0) - was calibration-frozen at v0.100.1; SUPERSEDED 2026-10-01/02 (branch merged into main, v1.0 now rests on the 0.101.0 suite); allowed-change rule, done list, post-1.0 items
metadata:
  type: project
---

**SUPERSEDED (2026-10-01/02):** the v0.100.1 suite (v2026-10.01) ran as a rehearsal and scored NO-GO,
so the v1.0 decision moved to the MOSAIC 0.101.0 suite (v2026-10.02, rubric 1.0.1; calibration-doctor
memory). release/v1.0's commits are merged (2849a8663 is an ancestor of main 1984d1713) and main is
0.101.0. The rubric pins the exact MOSAIC version, so a calibration-relevant change during the suite
needs a new version and a registered amendment. The post-1.0 list below still stands.

**Constraint (from the orchestrator, 2026-10-01, for the 0.100.1 plan):** v1.0 = the v0.100.1 code the dugong
production calibration suite is running, plus ONLY changes that cannot alter any
simulation, likelihood, sampling, weighting, ensemble, prior/config data object or
calibration artifact (NEWS/docs/roxygen, plot-only code, test hygiene). DESCRIPTION
stays 0.100.1 until the suite passes its acceptance gate; then the 1.0.0 bump renames
NEWS "# MOSAIC (development version)" to "# MOSAIC 1.0.0". Not pushed.

**Why:** the suite's results must describe the code that ships as 1.0.

**How to apply:** any fix touching R/sim_*, run_MOSAIC*, calc_*/likelihood, sample_*,
est_*/fit_*, data-raw/, data/, inst/extdata goes on the post-1.0 list, not the branch.
Prove freeze with [[calibration-freeze-verification]] before any further release commit.

Done on the branch (8 commits 3d13ff281..c93b870aa; d7a5f8e77 revises 3d13ff281's daily clustering to weekly): seasonal figure years derived from
data; plot_seasonal_clustering reads est_seasonal_dynamics daily fits (weekly means);
CFR Beta adaptive grid + sqrt y; vaccine density panels labelled data fits;
test-get_ggplot_legend + test-plot_model_likelihood Rplots.pdf; 0.100.1 NEWS claims re-measured.

Post-1.0 (owner in brackets): priors_default$metadata$description repeats the two
false claims - fix make_priors_default.R text at next rebuild [disease-modeler];
p_beta realized 95% 0.144-0.596 vs intended 0.10-0.50 (one-parameter mode fit) [dm/stat];
MOSAIC-docs regen of seasonal_*, case_fatality_ratio_beta_distributions (caption still
says AFRO "too narrow to show"), vaccine_all_combined after merge [docs];
plot_vaccine_effectiveness y "Density" is grid-normalised mass [maint]. Open TRACKER
calibration items stay deferred (claude/deep_review/TRACKER.md).
