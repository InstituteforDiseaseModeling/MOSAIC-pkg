---
name: surveillance-reconstruction-traps
description: Spread/rescaled surveillance cells distort NB dispersion and the per-cell likelihood (uniform windows inflate k and enforce flatness; sub-0.5 rescaled rows become weighted zeros); how to audit AI fourier provenance
metadata:
  type: reference
---

Measured on the fix/surveillance-artifacts red-team (2026-10-01, MOSAIC-data 7954843):

- **Uniform catch-up windows inflate est_nb_dispersion k.** Smooth weekly values -> low residual
  variance -> larger k for the WHOLE location. Joint k: ZAF 0.10 -> 3.82, GHA 0.14 -> 0.56,
  NGA 0.51 -> 1.54, KEN 0.16 -> 0.40. Excluding reconstructed+imputed cells: GHA 0.31, KEN 0.28.
  Reconstructions should not feed the dispersion estimator.
- **High k turns a uniform spread into a hard flat-shape constraint.** ZAF 2023 window (eff. weight
  0.552): an after-action-review-shaped trajectory with the SAME total loses 509 LL units vs flat
  at k=3.82 (101 at the old k=0.1). Seed-SD of scores is ~120, so this dominates selection.
- **Rescaling imputed rows to fractions manufactures zeros.** downscale_weekly_values(integer=TRUE)
  rounds each week, so rows < 0.5 become 0-case days that keep their weight. CIV 2025 (8.1 cases
  over 40 weeks): 39 weighted zero-weeks flip k to Inf (Poisson); masking them gives k = 0.71.
  Sparse-series k is fragile (CIV also flips on 14 window cells).
- **who_afro_annual 2023+ rows are dashboard sums**, the same source as the WHO weekly rows.
  year_fraction is the outbreak's duration (a country is listed only while active), not coverage.
  SOM is EMRO, so it has no AFRO annual row and cannot be reconciled against one.
- **Auditing AI fourier rows:** `git -C ai-cholera-data-mining show <commit <= snapshot date>:data/<ISO>/cholera_data_ai.csv`
  gives TL/TR/sCh plus notes. A fourier row with no AI record (LBR 2023) can come from
  `cholera_data_jhu.csv`, which holds the "WHO Annual Cholera Report" rows. The AI's own records
  often document zero transmission where its fourier puts cases (SSD Apr 2023-Sep 2024, AGO 2023).

Related: [[nb-dispersion-estimator-traps]], [[daily-vs-weekly-cases-scoring]], [[surveillance-artifacts-redteam]]
