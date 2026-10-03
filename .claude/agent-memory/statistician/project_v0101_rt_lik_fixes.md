---
name: v0101-rt-lik-fixes
description: v0.101.0 release red-team likelihood fixes (fix/v0101-rt-lik, 2026-10-01) - weekly-gate NA rule, failed-path -Inf, one-year deaths phi (on shipped v6.1: CAF 1->1.65, NER 1->2.97), deaths NB weekly + every-week k fallback via SPLICE (re-estimating the panel moves other rows through shrinkage), real-pool channel balance 4.8x not 5-7x, robust qualitative tests
metadata:
  type: project
---

**Invariants and traps found while fixing the v0.101.0 release red-team (branch fix/v0101-rt-lik).**

- **A gate change must be mirrored in every "nothing to score" guard (lesson-13 shape).**
  - The weekly core needs 3 scored weeks, while the NA rule kept the per-day have_cases gate (3 days).
  - A cases-only short location therefore scored a constant 0, and a single-location run got uniform weights.
  - Rule now: a channel counts when its core was scored, or when have_* holds and a shape term is on.
  - The run_MOSAIC pre-flight cannot see dates or settings, so it uses a date-free bound: the most complete 7-day blocks over the 7 alignments. That bound is never below the true count, so the check stays sufficient.
- **Dropping a week with a non-finite simulated value REWARDS a failed path**: it removes the week's negative term, or gives a constant 0 when every week drops.
  - The weekly cores return -Inf instead, counting only weeks with positive confidence weight.
- **glm cannot code a one-level year factor.**
  - Every one-year deaths window fell to phi = 1, the tightest kernel.
  - The fix fits an intercept-only D ~ offset(log C) model. Its closed form is r = sum D / sum C, phi = Pearson/(n-1).
  - glm's IRLS stops about 1e-6 (relative) short of the closed form, so test tolerance must be 1e-5.
  - config_default v6.0 at burn-in 45 and 30: only CAF (1 -> 1.715) and NER (1 -> 2.970) move (shipped v6.1: CAF 1.65, NER 2.97, per NEWS).
- **Re-running est_nb_dispersion() with altered tiers moves OTHER locations.**
  - Their k shifts because the shrinkage trend is refit with the new rows.
  - On the 40-location panel, re-running moved the deaths k of 19 other locations (BDI 1.43 -> 1.17).
  - Fix: re-estimate only the fallback rows, alone with shrink = FALSE, and splice them back in. Their value is then the same at every scale, and the other rows are byte-identical.
- **Channel balance on REAL draws** (14 v0.100.1 re-selection pools in claude/stat_v0100/reselect, with per-iteration daily/weekly cases and deaths scores):
  - The cases-score spread across draws, daily/weekly, is a median of 4.8 (1.8-6.5) for unchanged-k countries, and 4.1 among the top 1,000.
  - There is no change in the Poisson limit (LBR, 1.0).
  - The shift runs the other way where the panel trend raises k (CMR 0.66, UGA 0.92).
  - Deaths' share of top-1,000 score variance goes from 0.038 to 0.161.
  - The synthetic red-team draws (daily Poisson noise around smoothed observations) gave 13.7-18.5 for KEN against 6.5 real, so they overstate the shift.
- **Qualitative single-pool tests are coin flips on a re-roll** (16 of 70 seeds failed).
  - Assert the median of 5 pools, with thresholds a single pool misses in at most 3 of 70 seeds.
  - The median then fails only if 3 of the 5 pools miss: P ~ 10p^3, about 1e-3 or less.
  - Pass week_offset in loops; otherwise cadence detection costs ~30 ms per call and dominates.
- **Short fixtures under weekly cores go NA**: a 12-day fixture has one complete week.
  - Old assertions of the form all.equal(score(a), score(b)) then pass vacuously as NA vs NA.
  - Guard such tests with is.finite(), or use cases_scoring = "daily".

Done after this branch: the run.log "dispersion from ..." line and the pre-flight call site in R/run_MOSAIC.R (19f8a08fc), and the MOSAIC-docs 04 one-year phi and 05 daily-rule wording (MOSAIC-docs 280d837/88232c0).

See [[daily-vs-weekly-cases-scoring]], [[nb-dispersion-estimator-traps]] and [[obs-level-predictive-v0101]].
